object EditAggrFrame: TEditAggrFrame
  Left = 0
  Top = 0
  Width = 310
  Height = 348
  TabOrder = 0
  OnResize = FrameResize
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 310
    Height = 348
    ActivePage = tsCommon
    Align = alClient
    TabOrder = 0
    object tsCommon: TTabSheet
      Caption = 'tsCommon'
      DesignSize = (
        302
        320)
      object Label1: TLabel
        Left = 1
        Top = 30
        Width = 18
        Height = 13
        Caption = #1058#1080#1087
      end
      object cmbControlAggregate: TComboBox
        Left = 40
        Top = 22
        Width = 262
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 0
        Text = ' < '#1053#1077#1090' >'
        OnChange = cmbControlAggregateChange
        Items.Strings = (
          ' < '#1053#1077#1090' >'
          #1044#1074#1080#1075#1072#1090#1077#1083#1100
          #1047#1072#1076#1074#1080#1078#1082#1072
          #1054#1090#1086#1087#1080#1090#1077#1083#1100)
      end
      object leManual: TLabeledEdit
        Left = 1
        Top = 96
        Width = 301
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 157
        EditLabel.Height = 13
        EditLabel.Caption = #1050#1072#1085#1072#1083' '#1073#1083#1086#1082#1080#1088#1086#1074#1082#1080' '#1072#1074#1090#1086#1084#1072#1090#1080#1082#1080
        LabelSpacing = 1
        ReadOnly = True
        TabOrder = 1
        OnDragDrop = leManualDragDrop
        OnDragOver = leManualDragOver
        OnKeyDown = leManualKeyDown
      end
      object leControl: TLabeledEdit
        Left = 1
        Top = 134
        Width = 301
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 94
        EditLabel.Height = 13
        EditLabel.Caption = #1050#1072#1085#1072#1083' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1103
        LabelSpacing = 1
        ReadOnly = True
        TabOrder = 2
        OnDragDrop = leManualDragDrop
        OnDragOver = leManualDragOver
        OnKeyDown = leManualKeyDown
      end
      object pcChannels: TPageControl
        Left = -1
        Top = 192
        Width = 303
        Height = 128
        ActivePage = TabSheet4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        object TabSheet2: TTabSheet
          Caption = #1044#1074#1080#1075#1072#1090#1077#1083#1100
          DesignSize = (
            295
            100)
          object leU1: TLabeledEdit
            Left = 16
            Top = 4
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 7
            EditLabel.Height = 13
            EditLabel.Caption = 'U'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 0
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
          object leI1: TLabeledEdit
            Left = 16
            Top = 28
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 4
            EditLabel.Height = 13
            EditLabel.Caption = 'I'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 1
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
          object leP1: TLabeledEdit
            Left = 16
            Top = 52
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 6
            EditLabel.Height = 13
            EditLabel.Caption = 'P'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 2
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
          object leA1: TLabeledEdit
            Left = 16
            Top = 76
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 7
            EditLabel.Height = 13
            EditLabel.Caption = 'A'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 3
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
        object TabSheet3: TTabSheet
          Caption = #1047#1072#1076#1074#1080#1078#1082#1072
          ImageIndex = 1
          DesignSize = (
            295
            100)
          object leP12: TLabeledEdit
            Left = 16
            Top = 8
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 12
            EditLabel.Height = 13
            EditLabel.Caption = 'P1'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 0
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
          object leP22: TLabeledEdit
            Left = 16
            Top = 32
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 12
            EditLabel.Height = 13
            EditLabel.Caption = 'P2'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 1
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
          object leQ2: TLabeledEdit
            Left = 16
            Top = 56
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 8
            EditLabel.Height = 13
            EditLabel.Caption = 'Q'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 2
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
        object TabSheet4: TTabSheet
          Caption = #1054#1090#1086#1087#1080#1090#1077#1083#1100
          ImageIndex = 2
          DesignSize = (
            295
            100)
          object leT3: TLabeledEdit
            Left = 16
            Top = 8
            Width = 275
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 6
            EditLabel.Height = 13
            EditLabel.Caption = 'T'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 0
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
      end
      object leName: TLabeledEdit
        Left = 64
        Top = 0
        Width = 243
        Height = 21
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = #1053#1072#1079#1074#1072#1085#1080#1077
        LabelPosition = lpLeft
        TabOrder = 4
        OnExit = leNameExit
        OnKeyDown = leNameKeyDown
      end
      object lePreset: TLabeledEdit
        Left = 1
        Top = 170
        Width = 301
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 42
        EditLabel.Height = 13
        EditLabel.Caption = #1059#1089#1090#1072#1074#1082#1072
        LabelSpacing = 1
        ReadOnly = True
        TabOrder = 5
        OnDragDrop = leManualDragDrop
        OnDragOver = leManualDragOver
        OnKeyDown = leManualKeyDown
      end
      object leResetAlarm: TLabeledEdit
        Left = 1
        Top = 61
        Width = 301
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 107
        EditLabel.Height = 13
        EditLabel.Caption = #1050#1072#1085#1072#1083' '#1089#1073#1088#1086#1089#1072' '#1072#1074#1072#1088#1080#1080
        LabelSpacing = 1
        ReadOnly = True
        TabOrder = 6
        OnDragDrop = leManualDragDrop
        OnDragOver = leManualDragOver
        OnKeyDown = leManualKeyDown
      end
    end
    object tsAddParams: TTabSheet
      Caption = 'tsAddParams'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline ControlOneAddPrmFrame1: TControlOneAddPrmFrame
        Left = 0
        Top = 0
        Width = 302
        Height = 71
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 302
        inherited Panel1: TPanel
          Width = 302
          ExplicitWidth = 302
          inherited eCaption: TEdit
            OnExit = ControlOneAddPrmFrame1eCaptionExit
          end
          inherited lePrm: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 26
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
      end
      inline ControlOneAddPrmFrame2: TControlOneAddPrmFrame
        Left = 0
        Top = 71
        Width = 302
        Height = 71
        Align = alTop
        TabOrder = 1
        ExplicitTop = 71
        ExplicitWidth = 302
        inherited Panel1: TPanel
          Width = 302
          ExplicitWidth = 302
          inherited eCaption: TEdit
            OnExit = ControlOneAddPrmFrame1eCaptionExit
          end
          inherited lePrm: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 26
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
      end
      inline ControlOneAddPrmFrame3: TControlOneAddPrmFrame
        Left = 0
        Top = 142
        Width = 302
        Height = 71
        Align = alTop
        TabOrder = 2
        ExplicitTop = 142
        ExplicitWidth = 302
        inherited Panel1: TPanel
          Width = 302
          ExplicitWidth = 302
          inherited eCaption: TEdit
            OnExit = ControlOneAddPrmFrame1eCaptionExit
          end
          inherited lePrm: TLabeledEdit
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -16
            EditLabel.ExplicitWidth = 26
            OnDragDrop = leManualDragDrop
            OnDragOver = leManualDragOver
            OnKeyDown = leManualKeyDown
          end
        end
      end
    end
  end
end
