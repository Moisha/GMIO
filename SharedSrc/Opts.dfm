object OptsDlg: TOptsDlg
  Left = 537
  Top = 329
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
  ClientHeight = 228
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    741
    228)
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TcxButton
    Left = 575
    Top = 196
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 527
  end
  object btCancel: TcxButton
    Left = 663
    Top = 196
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 615
  end
  object GroupBox1: TcxGroupBox
    Left = 345
    Top = 8
    Caption = ' '#1054#1087#1094#1080#1080' '
    TabOrder = 2
    Height = 177
    Width = 393
    object Label1: TcxLabel
      Left = 197
      Top = 20
      Caption = #1043#1083#1091#1073#1080#1085#1072' '#1075#1088#1072#1092#1080#1082#1072' ('#1095#1072#1089'.)'
      Transparent = True
    end
    object Label7: TcxLabel
      Left = 202
      Top = 51
      Caption = #1042#1099#1089#1086#1090#1072' '#1086#1082#1085#1072' '#1089#1086#1073#1099#1090#1080#1081
      Transparent = True
    end
    object Label8: TcxLabel
      Left = 125
      Top = 79
      Caption = #1044#1086#1087#1091#1089#1090#1080#1084#1072#1103' '#1079#1072#1076#1077#1088#1078#1082#1072' '#1076#1072#1085#1085#1099#1093' ('#1084#1080#1085'.)'
      Transparent = True
    end
    object Label9: TcxLabel
      Left = 16
      Top = 120
      Caption = #1052#1077#1090#1086#1076' '#1086#1087#1088#1086#1089#1072
      Transparent = True
    end
    object seDiagramTime: TcxSpinEdit
      Left = 320
      Top = 19
      Properties.MaxValue = 48.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 0
      Value = 12
      Width = 57
    end
    object seAlarmsHeight: TcxSpinEdit
      Left = 320
      Top = 50
      Properties.MaxValue = 500.000000000000000000
      Properties.MinValue = 10.000000000000000000
      TabOrder = 1
      Value = 100
      Width = 57
    end
    object seNoRefreshTimeout: TcxSpinEdit
      Left = 320
      Top = 78
      Properties.MaxValue = 60.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 2
      Value = 4
      Width = 57
    end
    object cmbReqMethod: TcxComboBox
      Left = 16
      Top = 139
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        #1041#1086#1083#1100#1096#1080#1084#1080' '#1087#1086#1088#1094#1080#1103#1084#1080' ('#1073#1099#1089#1090#1088#1099#1081' '#1086#1087#1088#1086#1089' '#1076#1083#1103' '#1089#1082#1086#1088#1086#1089#1090#1085#1086#1081' '#1089#1074#1103#1079#1080')'
        #1052#1072#1083#1099#1084#1080' '#1087#1086#1088#1094#1080#1103#1084#1080' ('#1084#1077#1076#1083#1077#1085#1085#1099#1081' '#1086#1087#1088#1086#1089' '#1076#1083#1103' '#1087#1083#1086#1093#1080#1093' '#1083#1080#1085#1080#1081', '#1057#1054#1052')')
      TabOrder = 3
      Width = 361
    end
  end
  object GroupBox2: TcxGroupBox
    Left = 4
    Top = 8
    Caption = ' '#1057#1086#1077#1076#1080#1085#1077#1085#1080#1077' '
    TabOrder = 3
    Height = 177
    Width = 331
    object Label2: TcxLabel
      Left = 16
      Top = 20
      Caption = #1057#1077#1088#1074#1077#1088
      Properties.Alignment.Horz = taRightJustify
      Transparent = True
      AnchorX = 57
    end
    object Label3: TcxLabel
      Left = 28
      Top = 52
      Caption = #1055#1086#1088#1090
      Properties.Alignment.Horz = taRightJustify
      Transparent = True
      AnchorX = 57
    end
    object Label4: TcxLabel
      Left = 22
      Top = 79
      Caption = #1051#1086#1075#1080#1085
      Properties.Alignment.Horz = taRightJustify
      Transparent = True
      AnchorX = 57
    end
    object Label5: TcxLabel
      Left = 15
      Top = 106
      Caption = #1055#1072#1088#1086#1083#1100
      Properties.Alignment.Horz = taRightJustify
      Transparent = True
      AnchorX = 57
    end
    object Label6: TcxLabel
      Left = 137
      Top = 51
      Caption = #1058#1072#1081#1084#1072#1091#1090#1099' ('#1084#1089')'
      Properties.Alignment.Horz = taRightJustify
      Transparent = True
      AnchorX = 215
    end
    object leSrvPort: TcxTextEdit
      Left = 63
      Top = 51
      TabOrder = 0
      Text = '65500'
      Width = 68
    end
    object leSrv: TcxTextEdit
      Left = 63
      Top = 19
      TabOrder = 1
      Width = 262
    end
    object leLogin: TcxTextEdit
      Left = 63
      Top = 78
      TabOrder = 2
      Width = 262
    end
    object lePassword: TcxTextEdit
      Left = 63
      Top = 105
      Properties.OnChange = lePasswordChange
      TabOrder = 3
      Width = 262
    end
    object pnTimeZone: TcxGroupBox
      Left = 3
      Top = 136
      Align = alBottom
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
      TabOrder = 4
      Visible = False
      ExplicitLeft = 5
      ExplicitTop = 138
      ExplicitWidth = 323
      Height = 31
      Width = 325
      object Label10: TcxLabel
        Left = 3
        Top = 5
        Caption = #1063#1072#1089#1086#1074#1086#1081' '#1087#1086#1103#1089
        Transparent = True
      end
      object cmbTimeZone: TcxComboBox
        Left = 84
        Top = 3
        Properties.DropDownListStyle = lsFixedList
        TabOrder = 0
        Width = 238
      end
    end
    object leTimeOut: TcxTextEdit
      Left = 221
      Top = 51
      TabOrder = 5
      Text = '2000'
      Width = 49
    end
    object eTimeout2: TcxTextEdit
      Left = 279
      Top = 51
      TabOrder = 6
      Text = '1000'
      Width = 46
    end
  end
end
