object EditPrmWithDevicesFrm: TEditPrmWithDevicesFrm
  Left = 0
  Top = 0
  Width = 504
  Height = 147
  TabOrder = 0
  object Bevel1: TdxBevel
    Left = 1
    Top = 9
    Width = 380
    Height = 108
  end
  object Label1: TcxLabel
    Left = 11
    Top = 21
    Caption = #1050#1072#1085#1072#1083
    Transparent = True
  end
  object lNPrm: TcxLabel
    Left = 120
    Top = 0
    Caption = 'lNPrm'
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clMaroon
    Style.Font.Height = -13
    Style.Font.Name = 'MS Sans Serif'
    Style.Font.Style = [fsBold]
    Style.IsFontAssigned = True
  end
  object Label2: TcxLabel
    Left = 296
    Top = 46
    Caption = #1054#1090#1086#1073#1088#1072#1078#1077#1085#1080#1077
    Transparent = True
  end
  object Label3: TcxLabel
    Left = 73
    Top = 89
    AutoSize = False
    Caption = #1040#1074#1072#1088#1080#1103' '#1085#1072#1089#1086#1089#1072
    Properties.WordWrap = True
    Transparent = True
    Height = 25
    Width = 57
  end
  object Label5: TcxLabel
    Left = 224
    Top = 45
    Caption = #1056#1072#1079#1088#1103#1076#1085#1086#1089#1090#1100
    Transparent = True
  end
  object lBarFloat: TcxLabel
    Left = 75
    Top = 92
    Caption = #1043#1080#1089#1090#1077#1088#1077#1079#1080#1089
    Transparent = True
  end
  object Label6: TcxLabel
    Left = 11
    Top = 45
    Caption = 'DiagramMin'
    Transparent = True
  end
  object Label7: TcxLabel
    Left = 83
    Top = 45
    Caption = 'DiagramMax'
    Transparent = True
  end
  object Label8: TcxLabel
    Left = 160
    Top = 45
    Caption = #1055#1086#1076#1087#1080#1089#1100
    Transparent = True
  end
  object ePumpDownParam: TcxTextEdit
    Left = 120
    Top = 89
    Properties.ReadOnly = True
    TabOrder = 7
    Text = 'ePumpDownParam'
    OnDblClick = eMainParamDblClick
    OnDragDrop = ePumpDownParamDragDrop
    OnDragOver = ePumpDownParamDragOver
    OnKeyDown = ePumpDownParamKeyDown
    Width = 257
  end
  object cbDiagram: TcxCheckBox
    Left = 8
    Top = 90
    Caption = #1043#1088#1072#1092#1080#1082
    TabOrder = 0
    Transparent = True
    OnClick = leDevMinExit
  end
  object leDiagramMin: TcxTextEdit
    Left = 11
    Top = 61
    TabOrder = 1
    OnExit = leDevMinExit
    Width = 62
  end
  object leDiagramMax: TcxTextEdit
    Left = 83
    Top = 61
    TabOrder = 2
    OnExit = leDevMinExit
    Width = 62
  end
  object lePrefix: TcxTextEdit
    Left = 160
    Top = 61
    TabOrder = 3
    OnExit = leDevMinExit
    Width = 49
  end
  object cmbImage: TcxComboBox
    Left = 296
    Top = 61
    Properties.DropDownListStyle = lsFixedList
    Properties.Items.Strings = (
      #1062#1080#1092#1088#1072
      #1053#1072#1089#1086#1089)
    TabOrder = 4
    OnExit = leDevMinExit
    Width = 81
  end
  object leBarFloat: TcxTextEdit
    Left = 136
    Top = 89
    TabOrder = 5
    OnExit = leDevMinExit
    Width = 97
  end
  object cmbDigits: TcxComboBox
    Left = 224
    Top = 61
    Properties.DropDownListStyle = lsFixedList
    Properties.Items.Strings = (
      #1040#1074#1090#1086
      '0'
      '1'
      '2'
      '3'
      '4')
    TabOrder = 6
    OnExit = leDevMinExit
    Width = 65
  end
  object eMainParam: TcxTextEdit
    Left = 56
    Top = 17
    Properties.ReadOnly = True
    TabOrder = 8
    Text = 'eMainParam'
    OnDblClick = eMainParamDblClick
    OnDragDrop = eMainParamDragDrop
    OnDragOver = eMainParamDragOver
    OnKeyDown = ePumpDownParamKeyDown
    Width = 321
  end
end
