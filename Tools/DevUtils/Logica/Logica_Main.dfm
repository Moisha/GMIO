object Form1: TForm1
  Left = 474
  Top = 262
  Width = 1299
  Height = 597
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1289
    563)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 1196
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 0
    Top = 80
    Width = 1189
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 0
    Top = 112
    Width = 1269
    Height = 449
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object LabeledEdit1: TLabeledEdit
    Left = 144
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Channel'
    TabOrder = 3
    Text = '0'
  end
  object LabeledEdit2: TLabeledEdit
    Left = 288
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'Parameter'
    TabOrder = 4
    Text = '63'
  end
  object CommonPrm: TButton
    Left = 424
    Top = 24
    Width = 75
    Height = 25
    Caption = 'CommonPrm'
    TabOrder = 5
    OnClick = CommonPrmClick
  end
  object leDev: TLabeledEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Device'
    TabOrder = 6
    Text = '0'
  end
  object Button2: TButton
    Left = 512
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Archive'
    TabOrder = 7
    OnClick = Button2Click
  end
end
