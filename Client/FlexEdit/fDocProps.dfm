object fmDocProps: TfmDocProps
  Left = 302
  Top = 151
  BorderStyle = bsDialog
  Caption = 'Document properties'
  ClientHeight = 306
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    419
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object panProps: TcxGroupBox
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
    Left = 8
    Top = 8
    Width = 403
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Label1: TcxLabel
Transparent = True
      Left = 20
      Top = 16
      Width = 20
      Height = 13
      Caption = 'Title'
    end
    object Label2: TcxLabel
Transparent = True
      Left = 20
      Top = 68
      Width = 44
      Height = 13
      Caption = 'Comment'
    end
    object Label3: TcxLabel
Transparent = True
      Left = 20
      Top = 168
      Width = 77
      Height = 13
      Caption = 'Document width'
    end
    object Label4: TcxLabel
Transparent = True
      Left = 132
      Top = 168
      Width = 81
      Height = 13
      Caption = 'Document height'
    end
    object Label5: TcxLabel
Transparent = True
      Left = 280
      Top = 16
      Width = 86
      Height = 13
      Caption = 'Document version'
    end
    object Label12: TcxLabel
Transparent = True
      Left = 244
      Top = 168
      Width = 112
      Height = 13
      Caption = 'Connectors minimal gap'
    end
    object edTitle: TcxTextEdit
      Left = 20
      Top = 32
      Width = 249
      Height = 21
      TabOrder = 0
    end
    object mmComment: TcxMemo
      Left = 20
      Top = 84
      Width = 361
      Height = 65
      TabOrder = 2
    end
    object sedWidth: TcxSpinEdit
      Left = 20
      Top = 184
      Width = 89
      Height = 21
      Properties.MaxValue = 99999.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValueType = vtFloat
      Value = 1.000000000000000000
      TabOrder = 3
    end
    object sedHeight: TcxSpinEdit
      Left = 132
      Top = 184
      Width = 89
      Height = 21
      Properties.MaxValue = 99999.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValueType = vtFloat
      Value = 1.000000000000000000
      TabOrder = 4
    end
    object edVersion: TcxTextEdit
      Left = 280
      Top = 32
      Width = 101
      Height = 21
      Properties.ReadOnly = True
      TabOrder = 1
      Text = 'edVersion'
    end
    object sedConnectorsMinGap: TcxSpinEdit
      Left = 244
      Top = 184
      Width = 113
      Height = 21
      Properties.MaxValue = 99999.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValueType = vtFloat
      Value = 1.000000000000000000
      TabOrder = 5
    end
    object chSaveAsBinary: TcxCheckBox
Transparent = True
      Left = 20
      Top = 220
      Width = 169
      Height = 17
      Caption = 'Save document in binary format'
      TabOrder = 6
    end
    object chKeepLink: TcxCheckBox
Transparent = True
      Left = 244
      Top = 220
      Width = 129
      Height = 17
      Hint = 
        'Keep linking when connector'#39's end point '#13#10'disconnects from any c' +
        'onnection point'
      Caption = 'Keep connector'#39's link'
      TabOrder = 7
    end
  end
  object bbOk: TcxButton
    Left = 252
    Top = 272
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
  end
  object bbCancel: TcxButton
    Left = 336
    Top = 272
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 2
  end
  object bbUserData: TcxButton
    Left = 8
    Top = 272
    Width = 85
    Height = 25
    Caption = 'User data ...'
    TabOrder = 3
    OnClick = bbUserDataClick
  end
end
