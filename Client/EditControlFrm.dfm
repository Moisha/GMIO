object EditControlFrame: TEditControlFrame
  Left = 395
  Top = 0
  Width = 294
  Height = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    286
    331)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TcxLabel
Transparent = True
    Left = 8
    Top = 8
    Width = 19
    Height = 13
    Caption = #1058#1080#1087
  end
  object cmbControlAggregate: TcxComboBox
    Left = 8
    Top = 24
    Width = 271
    Height = 21
    Properties.DropDownListStyle = lsFixedList
    Anchors = [akLeft, akTop, akRight]
    Properties.ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = ' < '#1053#1077#1090' >'
    Properties.OnChange = cmbControlAggregateChange
    Properties.Items.Strings = (
      ' < '#1053#1077#1090' >'
      #1044#1074#1080#1075#1072#1090#1077#1083#1100
      #1047#1072#1076#1074#1080#1078#1082#1072
      #1054#1090#1086#1087#1080#1090#1077#1083#1100)
  end
  object leManual: TLabeledEdit
    Left = 8
    Top = 64
    Width = 271
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 135
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1072#1085#1072#1083' '#1088#1091#1095#1085#1086#1075#1086' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1103
    TabOrder = 1
    OnDblClick = leManualDblClick
    OnDragDrop = leA1DragDrop
    OnDragOver = leA1DragOver
    OnKeyDown = leManualKeyDown
  end
  object leControl: TLabeledEdit
    Left = 8
    Top = 104
    Width = 271
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 93
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1072#1085#1072#1083' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1103
    TabOrder = 2
    OnDblClick = leManualDblClick
    OnDragDrop = leA1DragDrop
    OnDragOver = leA1DragOver
    OnKeyDown = leManualKeyDown
  end
  object pcChannels: TcxPageControl
    Left = 8
    Top = 136
    Width = 273
    Height = 193
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    object TabSheet1: TcxTabSheet
      Caption = #1044#1074#1080#1075#1072#1090#1077#1083#1100
      DesignSize = (
        265
        165)
      object leU1: TLabeledEdit
        Left = 8
        Top = 24
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 8
        EditLabel.Height = 13
        EditLabel.Caption = 'U'
        TabOrder = 0
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
        OnKeyDown = leManualKeyDown
      end
      object leI1: TLabeledEdit
        Left = 8
        Top = 64
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 3
        EditLabel.Height = 13
        EditLabel.Caption = 'I'
        TabOrder = 1
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
        OnKeyDown = leManualKeyDown
      end
      object leP1: TLabeledEdit
        Left = 8
        Top = 104
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 7
        EditLabel.Height = 13
        EditLabel.Caption = 'P'
        TabOrder = 2
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
        OnKeyDown = leManualKeyDown
      end
      object leA1: TLabeledEdit
        Left = 8
        Top = 144
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 7
        EditLabel.Height = 13
        EditLabel.Caption = 'A'
        TabOrder = 3
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
        OnKeyDown = leManualKeyDown
      end
    end
    object TabSheet2: TcxTabSheet
      Caption = #1047#1072#1076#1074#1080#1078#1082#1072
      ImageIndex = 1
      DesignSize = (
        265
        165)
      object leP12: TLabeledEdit
        Left = 8
        Top = 24
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 13
        EditLabel.Height = 13
        EditLabel.Caption = 'P1'
        TabOrder = 0
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
      end
      object leP22: TLabeledEdit
        Left = 8
        Top = 64
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 13
        EditLabel.Height = 13
        EditLabel.Caption = 'P2'
        TabOrder = 1
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
      end
      object leQ2: TLabeledEdit
        Left = 8
        Top = 104
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 8
        EditLabel.Height = 13
        EditLabel.Caption = 'Q'
        TabOrder = 2
        OnDblClick = leManualDblClick
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
      end
    end
    object TabSheet3: TcxTabSheet
      Caption = #1054#1090#1086#1087#1080#1090#1077#1083#1100
      ImageIndex = 2
      DesignSize = (
        265
        165)
      object leT3: TLabeledEdit
        Left = 8
        Top = 24
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 7
        EditLabel.Height = 13
        EditLabel.Caption = 'T'
        TabOrder = 0
        OnDragDrop = leA1DragDrop
        OnDragOver = leA1DragOver
      end
    end
  end
end
