object Form1: TForm1
  Left = 389
  Top = 268
  Width = 442
  Height = 257
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 192
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 1
    OnClick = Button2Click
  end
  object eAddr: TEdit
    Left = 56
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '2109'
  end
  object eReadVal: TEdit
    Left = 280
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'eReadVal'
  end
  object eWriteVal: TEdit
    Left = 56
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'eWriteVal'
  end
  object Memo1: TMemo
    Left = 56
    Top = 104
    Width = 185
    Height = 89
    Lines.Strings = (
      '2109 - '#1085#1072#1087#1088#1103#1078#1077#1085#1080#1077
      '2000 - '#1087#1091#1089#1082'/'#1089#1090#1086#1087
      '2002 - '#1095#1072#1089#1090#1086#1090#1072)
    TabOrder = 5
  end
end
