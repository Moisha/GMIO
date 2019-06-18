object Form1: TForm1
  Left = 396
  Top = 177
  Caption = 'Form1'
  ClientHeight = 529
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 2
    Top = 72
    Width = 697
    Height = 319
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = Memo1KeyDown
  end
  object Button1: TButton
    Left = 2
    Top = 401
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 122
    Top = 409
    Width = 97
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Activate'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object Button2: TButton
    Left = 362
    Top = 401
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'CountStrings'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 7
    Top = 20
    Width = 683
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 4
    Text = '10 40 0 11 1 1C 01'
    OnKeyPress = Edit1KeyPress
  end
  object IdUDPServer1: TIdUDPServer
    OnStatus = IdUDPServer2Status
    Active = True
    Bindings = <>
    DefaultPort = 65502
    OnUDPRead = IdUDPServer1UDPRead
    Left = 80
    Top = 72
  end
  object IdUDPClient1: TIdUDPClient
    Active = True
    Host = '80.78.118.22'
    Port = 51960
    ReceiveTimeout = 3000
    Left = 80
    Top = 112
  end
end
