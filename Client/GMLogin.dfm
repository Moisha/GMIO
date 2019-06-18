object LoginDlg: TLoginDlg
  Left = 955
  Top = 383
  BorderIcons = [biSystemMenu]
  Caption = #1044#1086#1089#1090#1091#1087' '#1082' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1102
  ClientHeight = 130
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TcxLabel
    Left = 27
    Top = 27
    Caption = #1051#1086#1075#1080#1085
    Transparent = True
  end
  object Label2: TcxLabel
    Left = 20
    Top = 59
    Caption = #1055#1072#1088#1086#1083#1100
    Transparent = True
  end
  object btOk: TcxButton
    Left = 72
    Top = 96
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Ok'
    Default = True
    TabOrder = 2
    OnClick = btOkClick
  end
  object Button2: TcxButton
    Left = 152
    Top = 96
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 3
  end
  object leLogin: TcxTextEdit
    Left = 64
    Top = 24
    TabOrder = 0
    Width = 161
  end
  object lePassword: TcxTextEdit
    Left = 64
    Top = 56
    Properties.PasswordChar = '*'
    TabOrder = 1
    Width = 161
  end
end
