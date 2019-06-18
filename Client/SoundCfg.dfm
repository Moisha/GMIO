object SoundCfgDlg: TSoundCfgDlg
  Left = 394
  Top = 222
  BorderStyle = bsSingle
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1079#1074#1091#1082#1072
  ClientHeight = 200
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    516
    200)
  PixelsPerInch = 96
  TextHeight = 13
  object Button2: TcxButton
    Left = 438
    Top = 169
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 0
  end
  object Button3: TcxButton
    Left = 358
    Top = 169
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object GroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Caption = ' '#1047#1074#1091#1082' '
    TabOrder = 2
    DesignSize = (
      513
      97)
    Height = 97
    Width = 513
    object Label1: TcxLabel
      Left = 11
      Top = 19
      Caption = #1060#1072#1081#1083
      Transparent = True
    end
    object cbNeedAlarm: TcxCheckBox
      Left = 48
      Top = 48
      Caption = #1057#1080#1075#1085#1072#1083#1080#1079#1072#1094#1080#1103' '#1086' '#1090#1088#1077#1074#1086#1075#1072#1093
      TabOrder = 0
      Transparent = True
    end
    object cbNeedNodata: TcxCheckBox
      Left = 48
      Top = 72
      Caption = #1057#1080#1075#1085#1072#1083#1080#1079#1072#1094#1080#1103' '#1086' '#1087#1088#1086#1087#1072#1076#1072#1085#1080#1080' '#1089#1074#1103#1079#1080
      TabOrder = 1
      Transparent = True
    end
    object leFilename: TcxTextEdit
      Left = 48
      Top = 16
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Width = 433
    end
    object Button1: TcxButton
      Left = 480
      Top = 16
      Width = 23
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = Button1Click
    end
  end
  object GroupBox2: TcxGroupBox
    Left = 0
    Top = 104
    Caption = ' '#1052#1080#1075#1072#1102#1097#1072#1103' '#1087#1072#1085#1077#1083#1100
    TabOrder = 3
    Height = 49
    Width = 513
    object Label2: TcxLabel
      Left = 11
      Top = 19
      Caption = #1064#1080#1088#1080#1085#1072
      Transparent = True
    end
    object leBlinkingPanelWidth: TcxTextEdit
      Left = 56
      Top = 16
      TabOrder = 0
      Width = 121
    end
  end
  object odMP3: TOpenDialog
    Filter = #1047#1074#1091#1082#1080' '#1074' '#1092#1086#1088#1084#1072#1090#1077' MP3 (*.mp3)|*.mp3|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofNoChangeDir, ofEnableSizing]
    Left = 464
    Top = 56
  end
end
