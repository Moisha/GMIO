object Form1: TForm1
  Left = 318
  Top = 232
  Width = 706
  Height = 483
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
  object mAnswer: TMemo
    Left = 8
    Top = 56
    Width = 681
    Height = 121
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'bReq'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 16
    Width = 75
    Height = 25
    Caption = 'CRC'
    TabOrder = 2
    OnClick = Button2Click
  end
  object eStrForCRC: TEdit
    Left = 288
    Top = 16
    Width = 57
    Height = 21
    TabOrder = 3
    Text = 'ind.t'
  end
  object mDecode: TMemo
    Left = 8
    Top = 248
    Width = 681
    Height = 137
    Lines.Strings = (
      'mDecode')
    TabOrder = 4
  end
  object Button3: TButton
    Left = 8
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Decode'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 368
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
  object eNumber: TEdit
    Left = 88
    Top = 16
    Width = 49
    Height = 21
    TabOrder = 7
    Text = '1'
  end
end
