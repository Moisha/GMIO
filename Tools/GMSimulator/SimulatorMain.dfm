object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 304
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnGo: TSpeedButton
    Left = 536
    Top = 8
    Width = 97
    Height = 34
    Caption = 'Go'
    OnClick = btnGoClick
  end
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 30
    Height = 13
    Caption = 'N_Car'
  end
  object Label2: TLabel
    Left = 96
    Top = 19
    Width = 29
    Height = 13
    Caption = 'Count'
  end
  object Label3: TLabel
    Left = 224
    Top = 19
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object Memo1: TMemo
    Left = 8
    Top = 64
    Width = 625
    Height = 233
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object eNCar: TEdit
    Left = 45
    Top = 16
    Width = 36
    Height = 21
    TabOrder = 1
    Text = '1'
  end
  object eCount: TEdit
    Left = 141
    Top = 16
    Width = 60
    Height = 21
    TabOrder = 2
    Text = '1000'
  end
  object Button1: TButton
    Left = 368
    Top = 14
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 3
    OnClick = Button1Click
  end
  object ePort: TEdit
    Left = 261
    Top = 16
    Width = 66
    Height = 21
    TabOrder = 4
    Text = '65501'
  end
end
