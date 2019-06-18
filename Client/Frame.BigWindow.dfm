inherited BigWindowFrame: TBigWindowFrame
  Width = 702
  Height = 416
  object pdDiagramLen: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    Height = 57
    Width = 702
    object rbDiagramLenHrs: TcxRadioGroup
      Left = 8
      Top = 8
      Caption = #1044#1083#1080#1085#1072' '#1075#1088#1072#1092#1080#1082#1072
      Properties.Columns = 5
      Properties.Items = <
        item
          Caption = '1 '#1095#1072#1089
        end
        item
          Caption = '3 '#1095#1072#1089#1072
        end
        item
          Caption = '8 '#1095#1072#1089#1086#1074
        end
        item
          Caption = '12 '#1095#1072#1089#1086#1074
        end
        item
          Caption = '24 '#1095#1072#1089#1072
        end>
      ItemIndex = 4
      TabOrder = 0
      OnClick = rbDiagramLenHrsClick
      Height = 39
      Width = 441
    end
    object cxLabel1: TcxLabel
      Left = 472
      Top = 24
      Caption = #1064#1072#1075' ('#1084#1080#1085'.)'
      Transparent = True
    end
    object cmbTimeStep: TcxComboBox
      Left = 536
      Top = 23
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        '1 '
        '3 '
        '5 '
        '10 '
        '15 '
        '30 '
        '60')
      TabOrder = 2
      Text = '5 '
      OnClick = cmbTimeStepPropertiesChange
      Width = 49
    end
  end
  inline DiagramAndTable: TFrmDiagramAndTable
    Left = 0
    Top = 57
    Width = 702
    Height = 359
    Align = alClient
    TabOrder = 0
    inherited Splitter1: TcxSplitter
      Width = 702
    end
    inherited Panel1: TcxGroupBox
      Width = 702
    end
    inherited vstTable: TVirtualStringTree
      Width = 702
      Height = 159
    end
  end
  object tmrRequestGraph: TTimer
    Interval = 500
    OnTimer = tmrRequestGraphTimer
    Left = 336
    Top = 192
  end
end
