object CheckConnStrDlg: TCheckConnStrDlg
  Left = 734
  Top = 447
  BorderIcons = []
  Caption = #1057#1086#1077#1076#1080#1085#1077#1085#1080#1077' '#1089' SQL'
  ClientHeight = 223
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lState: TLabel
    Left = 16
    Top = 29
    Width = 497
    Height = 148
    AutoSize = False
    Caption = 'Connecting...'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Caption = #1057#1090#1072#1090#1091#1089':'
  end
  object Button1: TButton
    Left = 448
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 0
  end
end
