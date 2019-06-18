object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 162
  ClientWidth = 224
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object leLastDT: TLabeledEdit
    Left = 8
    Top = 40
    Width = 121
    Height = 21
    EditLabel.Width = 70
    EditLabel.Height = 13
    EditLabel.Caption = 'Last write time'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 80
    Top = 96
    Width = 137
    Height = 25
    Caption = #1053#1072#1095#1072#1083#1100#1085#1099#1077' '#1076#1072#1085#1085#1099#1077
    TabOrder = 1
    OnClick = Button1Click
  end
  object eInitialHours: TEdit
    Left = 8
    Top = 98
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '2'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 136
    Width = 209
    Height = 16
    TabOrder = 3
  end
  object Timer1: TTimer
    Interval = 60000
    OnTimer = Timer1Timer
    Left = 160
    Top = 32
  end
end
