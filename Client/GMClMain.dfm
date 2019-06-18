object GMClMainFrm: TGMClMainFrm
  Left = 600
  Top = 308
  ClientHeight = 690
  ClientWidth = 996
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TcxGroupBox
    Left = 0
    Top = 21
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    Height = 649
    Width = 996
    object dxDockSiteMain: TdxDockSite
      Left = 0
      Top = 0
      Width = 996
      Height = 649
      Align = alClient
      DockingType = 5
      OriginalWidth = 996
      OriginalHeight = 649
      object dxLayoutDockSite1: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 996
        Height = 649
        DockingType = 0
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object dxVertContainerDockSite1: TdxVertContainerDockSite
        Left = 0
        Top = 0
        Width = 996
        Height = 649
        ActiveChildIndex = -1
        AllowFloating = True
        AutoHide = False
        CustomCaptionButtons.Buttons = <>
        DockingType = 0
        OriginalWidth = 826
        OriginalHeight = 438
        object dxDockPanelObjects: TdxDockPanel
          Left = 0
          Top = 0
          Width = 996
          Height = 538
          ManagerFont = False
          ParentColor = True
          ParentFont = True
          AllowFloating = True
          AutoHide = False
          Caption = #1054#1073#1098#1077#1082#1090#1099
          CaptionButtons = [cbMaximize, cbClose]
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CloseButtonMode = cbmEveryTab
          TabsProperties.CustomButtons.Buttons = <>
          TabsProperties.TabPosition = tpTop
          OnCloseQuery = dxDockPanelEventsCloseQuery
          DockingType = 2
          OriginalWidth = 826
          OriginalHeight = 538
          object pnControl: TcxGroupBox
            Left = 688
            Top = 0
            Align = alRight
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            Style.TransparentBorder = False
            TabOrder = 0
            Visible = False
            Height = 508
            Width = 300
            inline ControlFrm1: TControlFrm
              Left = 0
              Top = 0
              Width = 300
              Height = 508
              Align = alClient
              TabOrder = 0
              inherited Panel1: TcxGroupBox
                inherited lObjName: TcxLabel
                  Style.IsFontAssigned = True
                  AnchorX = 135
                end
              end
              inherited pnAggregates: TcxGroupBox
                inherited Aggregate1: TControlOneAggregateFrame
                  inherited lbName: TcxLabel
                    Style.IsFontAssigned = True
                    AnchorX = 50
                  end
                  inherited LabelParams: TcxLabel
                    Style.IsFontAssigned = True
                  end
                  inherited Label1: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label2: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label3: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label4: TcxLabel
                    AnchorX = 45
                  end
                end
                inherited Aggregate2: TControlOneAggregateFrame
                  inherited lbName: TcxLabel
                    Style.IsFontAssigned = True
                    AnchorX = 50
                  end
                  inherited LabelParams: TcxLabel
                    Style.IsFontAssigned = True
                  end
                  inherited Label1: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label2: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label3: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label4: TcxLabel
                    AnchorX = 45
                  end
                end
                inherited Aggregate3: TControlOneAggregateFrame
                  inherited lbName: TcxLabel
                    Style.IsFontAssigned = True
                    AnchorX = 50
                  end
                  inherited LabelParams: TcxLabel
                    Style.IsFontAssigned = True
                  end
                  inherited Label1: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label2: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label3: TcxLabel
                    AnchorX = 45
                  end
                  inherited Label4: TcxLabel
                    AnchorX = 45
                  end
                end
              end
              inherited pnAlarm: TcxGroupBox
                Height = 159
                inherited mAlarms: TcxMemo
                  Height = 159
                end
              end
            end
          end
          object ScrollObjFrames: TcxScrollBox
            Left = 0
            Top = 0
            Width = 688
            Height = 508
            Align = alClient
            TabOrder = 1
            object lConnectionTest: TcxLabel
              Left = 16
              Top = 16
              Caption = #1057#1086#1077#1076#1080#1085#1077#1085#1080#1077' '#1089' '#1089#1077#1088#1074#1077#1088#1086#1084' ...'
              ParentFont = False
              Style.Font.Charset = DEFAULT_CHARSET
              Style.Font.Color = clWindowText
              Style.Font.Height = -27
              Style.Font.Name = 'Times New Roman'
              Style.Font.Style = []
              Style.IsFontAssigned = True
              Transparent = True
            end
          end
        end
        object dxDockPanelEvents: TdxDockPanel
          Left = 0
          Top = 538
          Width = 996
          Height = 111
          AllowFloating = True
          AutoHide = False
          Caption = #1057#1086#1073#1099#1090#1080#1103
          CaptionButtons = [cbClose]
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          OnCloseQuery = dxDockPanelEventsCloseQuery
          DockingType = 2
          OriginalWidth = 954
          OriginalHeight = 111
          object pnAlarms: TcxGroupBox
            Left = 0
            Top = 0
            Align = alClient
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            Style.TransparentBorder = False
            TabOrder = 0
            Height = 81
            Width = 988
            object lbAlarms: TcxListBox
              Left = 0
              Top = 0
              Width = 890
              Height = 81
              Align = alClient
              ItemHeight = 13
              ListStyle = lbOwnerDrawFixed
              TabOrder = 0
              OnDblClick = lbAlarmsDblClick
              OnDrawItem = lbAlarmsDrawItem
            end
            object pnBlinkAlarm: TcxGroupBox
              Left = 890
              Top = 0
              Align = alRight
              PanelStyle.Active = True
              Style.BorderStyle = ebsNone
              Style.TransparentBorder = False
              TabOrder = 1
              Visible = False
              Height = 81
              Width = 98
            end
          end
        end
      end
    end
  end
  object Statusbar1: TdxStatusBar
    Left = 0
    Top = 670
    Width = 996
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        PanelStyle.Alignment = taRightJustify
        Width = 150
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 20
      end>
    PaintStyle = stpsUseLookAndFeel
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OnResize = StatusBar1Resize
  end
  object tmrCheckUpdates: TTimer
    Interval = 10000
    OnTimer = tmrCheckUpdatesTimer
    Left = 44
    Top = 112
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 432
    Top = 80
  end
  object tmrUpdateTime: TTimer
    OnTimer = tmrUpdateTimeTimer
    Left = 44
    Top = 152
  end
  object tmrBlinkAlarms: TTimer
    Interval = 500
    OnTimer = tmrBlinkAlarmsTimer
    Left = 44
    Top = 192
  end
  object tmrScript: TTimer
    Left = 44
    Top = 240
  end
  object cxLocalizer1: TcxLocalizer
    Active = True
    StorageType = lstResource
    Left = 280
    Top = 199
  end
  object dxSkinController1: TdxSkinController
    NativeStyle = False
    SkinName = 'Office2010Silver'
    OnSkinControl = dxSkinController1SkinControl
    OnSkinForm = dxSkinController1SkinForm
    Left = 360
    Top = 199
  end
  object dxDockingManager1: TdxDockingManager
    Color = clBtnFace
    DefaultHorizContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultHorizContainerSiteProperties.Dockable = True
    DefaultHorizContainerSiteProperties.ImageIndex = -1
    DefaultVertContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultVertContainerSiteProperties.Dockable = True
    DefaultVertContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultTabContainerSiteProperties.Dockable = False
    DefaultTabContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.TabsProperties.CloseButtonMode = cbmActiveTab
    DefaultTabContainerSiteProperties.TabsProperties.CustomButtons.Buttons = <>
    DefaultTabContainerSiteProperties.TabsProperties.TabPosition = tpTop
    DockStyle = dsVS2005
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [doActivateAfterDocking, doFloatingOnTop, doTabContainerCanAutoHide, doSideContainerCanClose, doSideContainerCanAutoHide, doTabContainerCanInSideContainer, doSideContainerCanInTabContainer, doSideContainerCanInSideContainer, doRedrawOnResize]
    Left = 466
    Top = 200
    PixelsPerInch = 96
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default'
      'MainMenu1'
      #1054#1090#1095#1077#1090#1099
      #1053#1072#1089#1090#1088#1086#1081#1082#1080
      #1056#1077#1078#1080#1084
      #1057#1082#1072#1076#1072)
    Categories.ItemsVisibles = (
      2
      2
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True
      True
      True)
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 200
    Top = 200
    DockControlHeights = (
      0
      0
      21
      0)
    object dxBarMainMenu: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Main Menu'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 0
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'N2'
        end
        item
          Visible = True
          ItemName = 'N3'
        end
        item
          Visible = True
          ItemName = 'N7'
        end
        item
          Visible = True
          ItemName = 'miWindowList'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object miWindowList: TdxBarSubItem
      Caption = #1054#1082#1085#1072
      Category = 0
      Visible = ivAlways
      OnClick = miWindowListClick
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButtonObjects'
        end
        item
          Visible = True
          ItemName = 'dxBarButtonEvents'
        end
        item
          Visible = True
          ItemName = 'N10'
        end
        item
          Visible = True
          ItemName = 'dxBarSubItemBigWindows'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButtonCloseAll'
        end>
    end
    object dxBarSubItemBigWindows: TdxBarSubItem
      Caption = #1043#1088#1072#1092#1080#1082#1080
      Category = 0
      Hint = #1043#1088#1072#1092#1080#1082#1080
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButtonObjects: TdxBarButton
      Caption = #1054#1073#1098#1077#1082#1090#1099
      Category = 0
      Hint = #1054#1073#1098#1077#1082#1090#1099
      Visible = ivAlways
      AllowAllUp = True
      ButtonStyle = bsChecked
      GroupIndex = 2
      OnClick = dxBarButtonObjectsClick
    end
    object dxBarButtonEvents: TdxBarButton
      Caption = #1057#1086#1073#1099#1090#1080#1103
      Category = 0
      Hint = #1057#1086#1073#1099#1090#1080#1103
      Visible = ivAlways
      AllowAllUp = True
      ButtonStyle = bsChecked
      GroupIndex = 3
      OnClick = dxBarButtonEventsClick
    end
    object dxBarButtonCloseAll: TdxBarButton
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1074#1089#1077
      Category = 0
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1074#1089#1077
      Visible = ivAlways
      OnClick = dxBarButtonCloseAllClick
    end
    object N2: TdxBarSubItem
      Caption = #1054#1090#1095#1077#1090#1099
      Category = 1
      Visible = ivAlways
      OnClick = N7Click
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miCommonRep'
        end
        item
          Visible = True
          ItemName = 'N5'
        end
        item
          Visible = True
          ItemName = 'miHydroGeoRep'
        end
        item
          Visible = True
          ItemName = 'N6'
        end>
    end
    object N3: TdxBarSubItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      Category = 1
      Visible = ivAlways
      OnClick = N7Click
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miConnectionSettings'
        end
        item
          Visible = True
          ItemName = 'N4'
        end
        item
          Visible = True
          ItemName = 'N9'
        end>
    end
    object N7: TdxBarSubItem
      Caption = #1056#1077#1078#1080#1084
      Category = 1
      Visible = ivAlways
      OnClick = N7Click
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miEditMode'
        end
        item
          Visible = True
          ItemName = 'miViewMode'
        end
        item
          Visible = True
          ItemName = 'miControlMode'
        end>
    end
    object N10: TdxBarSubItem
      Caption = #1057#1082#1072#1076#1072
      Category = 1
      Visible = ivAlways
      OnClick = N7Click
      ItemLinks = <
        item
          Visible = True
          ItemName = 'N11'
        end
        item
          Visible = True
          ItemName = 'miLoadScada'
        end>
    end
    object miCommonRep: TdxBarButton
      Caption = #1040#1088#1093#1080#1074' '#1076#1072#1085#1085#1099#1093
      Category = 2
      Visible = ivAlways
      OnClick = miCommonRepClick
    end
    object N5: TdxBarButton
      Caption = #1046#1091#1088#1085#1072#1083' '#1072#1074#1072#1088#1080#1081
      Category = 2
      Visible = ivAlways
      OnClick = N5Click
    end
    object miHydroGeoRep: TdxBarButton
      Caption = #1054#1090#1095#1077#1090' '#1075#1080#1076#1088#1086#1075#1077#1086#1083#1086#1075#1072
      Category = 2
      Visible = ivAlways
      OnClick = miHydroGeoRepClick
    end
    object N6: TdxBarButton
      Caption = #1064#1072#1073#1083#1086#1085#1085#1099#1077' '#1086#1090#1095#1077#1090#1099
      Category = 2
      Visible = ivAlways
      OnClick = N6Click
    end
    object miConnectionSettings: TdxBarButton
      Caption = #1057#1074#1103#1079#1100' '#1080' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1040#1056#1052#1072
      Category = 3
      Visible = ivAlways
      OnClick = miConnectionSettingsClick
    end
    object N4: TdxBarButton
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1086#1073#1098#1077#1082#1090#1086#1074
      Category = 3
      Visible = ivAlways
      OnClick = N4Click
    end
    object N9: TdxBarButton
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1087#1086#1074#1077#1097#1077#1085#1080#1103' '#1086' '#1090#1088#1077#1074#1086#1075#1072#1093
      Category = 3
      Visible = ivAlways
      OnClick = N9Click
    end
    object N1: TdxBarButton
      Caption = #1046#1091#1088#1085#1072#1083
      Category = 3
      Visible = ivNever
    end
    object miViewMode: TdxBarButton
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088
      Category = 4
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      OnClick = miControlModeClick
    end
    object miControlMode: TdxBarButton
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      Category = 4
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      OnClick = miControlModeClick
    end
    object miEditMode: TdxBarButton
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077
      Category = 4
      Hint = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      OnClick = miControlModeClick
    end
    object N11: TdxBarButton
      Caption = #1057#1086#1079#1076#1072#1090#1100
      Category = 5
      Visible = ivAlways
      OnClick = N11Click
    end
    object miLoadScada: TdxBarButton
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      Category = 5
      Visible = ivAlways
      OnClick = miLoadScadaClick
    end
  end
  object odScada: TOpenDialog
    Filter = #1060#1072#1081#1083#1099' '#1084#1085#1077#1084#1086#1089#1093#1077#1084' (*.fxd)|*.fxd|*.*|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 272
    Top = 56
  end
end
