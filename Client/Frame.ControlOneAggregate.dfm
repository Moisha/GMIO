object ControlOneAggregateFrame: TControlOneAggregateFrame
  Left = 0
  Top = 0
  Width = 105
  Height = 242
  TabOrder = 0
  object Bevel1: TdxBevel
    Left = 0
    Top = 0
    Width = 100
    Height = 236
  end
  object btnStart: TcxButton
    Left = 7
    Top = 64
    Width = 42
    Height = 25
    Caption = #1055#1059#1057#1050
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 4
    OnClick = btnManualClick
  end
  object btnStop: TcxButton
    Left = 52
    Top = 64
    Width = 42
    Height = 25
    Caption = #1057#1058#1054#1055
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 5
    OnClick = btnManualClick
  end
  object lbName: TcxLabel
    Left = 1
    Top = 4
    AutoSize = False
    Caption = #1044#1074#1080#1075#1072#1090#1077#1083#1100
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -11
    Style.Font.Name = 'MS Sans Serif'
    Style.Font.Style = [fsBold]
    Style.IsFontAssigned = True
    Properties.Alignment.Horz = taCenter
    Properties.WordWrap = True
    Transparent = True
    Height = 24
    Width = 97
    AnchorX = 50
  end
  object LabelParams: TcxLabel
    Left = 17
    Top = 121
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -11
    Style.Font.Name = 'MS Sans Serif'
    Style.Font.Style = [fsBold]
    Style.IsFontAssigned = True
    Transparent = True
  end
  object btnPreset: TcxButton
    Left = 7
    Top = 33
    Width = 87
    Height = 26
    Caption = #1059#1089#1090#1072#1074#1082#1072
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 8
    OnClick = btnManualClick
  end
  object btnResetAlarm: TcxButton
    Left = 8
    Top = 92
    Width = 86
    Height = 26
    Caption = #1057#1073#1088#1086#1089' '#1072#1074#1072#1088#1080#1080
    SpeedButtonOptions.CanBeFocused = False
    TabOrder = 9
    OnClick = btnManualClick
  end
  object Label1: TcxLabel
    Left = 10
    Top = 140
    Caption = 'Label1'
    Properties.Alignment.Horz = taRightJustify
    Transparent = True
    AnchorX = 45
  end
  object Label2: TcxLabel
    Left = 10
    Top = 164
    Caption = 'Label2'
    Properties.Alignment.Horz = taRightJustify
    Transparent = True
    AnchorX = 45
  end
  object Label3: TcxLabel
    Left = 10
    Top = 188
    Caption = 'Label3'
    Properties.Alignment.Horz = taRightJustify
    Transparent = True
    AnchorX = 45
  end
  object Label4: TcxLabel
    Left = 10
    Top = 212
    Caption = 'Label4'
    Properties.Alignment.Horz = taRightJustify
    Transparent = True
    AnchorX = 45
  end
  object le1: TcxTextEdit
    Left = 50
    Top = 137
    Properties.ReadOnly = True
    TabOrder = 0
    Width = 41
  end
  object le2: TcxTextEdit
    Left = 50
    Top = 161
    Properties.ReadOnly = True
    TabOrder = 1
    Width = 41
  end
  object le3: TcxTextEdit
    Left = 50
    Top = 185
    Properties.ReadOnly = True
    TabOrder = 2
    Width = 41
  end
  object le4: TcxTextEdit
    Left = 50
    Top = 209
    Properties.ReadOnly = True
    TabOrder = 3
    Width = 41
  end
end
