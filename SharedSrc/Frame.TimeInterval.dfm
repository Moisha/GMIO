object frmSelArchTime: TfrmSelArchTime
  Left = 0
  Top = 0
  Width = 464
  Height = 33
  TabOrder = 0
  object Label2: TcxLabel
    Left = 165
    Top = 10
    Caption = '--'
    Transparent = True
  end
  object Label1: TcxLabel
    Left = 2
    Top = 12
    Caption = #1055#1077#1088#1080#1086#1076
    Properties.Alignment.Horz = taRightJustify
    Transparent = True
    AnchorX = 44
  end
  object bPrev: TcxButton
    Left = 303
    Top = 5
    Width = 23
    Height = 22
    Caption = '<<'
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 5
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = bPrevClick
  end
  object bNext: TcxButton
    Left = 332
    Top = 5
    Width = 23
    Height = 22
    Caption = '>>'
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = bPrevClick
  end
  object dcbDate1: TcxDateEdit
    Left = 47
    Top = 6
    Properties.Kind = ckDateTime
    Properties.OnChange = dcbDate1Change
    TabOrder = 0
    Width = 114
  end
  object dcbDate2: TcxDateEdit
    Left = 177
    Top = 6
    Properties.Kind = ckDateTime
    Properties.OnChange = dcbDate1Change
    TabOrder = 1
    Width = 120
  end
  object cmbTimeStamp: TcxComboBox
    Left = 361
    Top = 6
    Properties.DropDownListStyle = lsFixedList
    Properties.Items.Strings = (
      #1059#1082#1072#1079#1072#1090#1100' ...'
      #1057#1091#1090#1082#1080
      #1053#1077#1076#1077#1083#1103
      #1052#1077#1089#1103#1094
      #1050#1074#1072#1088#1090#1072#1083
      #1055#1086#1083#1075#1086#1076#1072
      #1043#1086#1076
      #1042#1089#1077#1075#1076#1072)
    Properties.OnChange = dcbDate1Change
    TabOrder = 2
    Width = 80
  end
end
