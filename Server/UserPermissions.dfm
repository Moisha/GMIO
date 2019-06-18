object UsersDlg: TUsersDlg
  Left = 0
  Top = 0
  Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1080
  ClientHeight = 583
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 663
    Height = 583
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DataModuleSkin.dxLayoutSkinLookAndFeel1
    object labelState: TcxLabel
      Left = 305
      Top = 10
      Caption = 'labelState'
      Style.HotTrack = False
      Transparent = True
    end
    object cmbUsers: TcxComboBox
      Left = 87
      Top = 35
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbUsersPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 121
    end
    object cxButton2: TcxButton
      Left = 497
      Top = 33
      Width = 75
      Height = 25
      Caption = #1059#1076#1072#1083#1080#1090#1100
      TabOrder = 3
      OnClick = cxButton2Click
    end
    object buttonAddUser: TcxButton
      Left = 416
      Top = 33
      Width = 75
      Height = 25
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 2
      OnClick = buttonAddUserClick
    end
    object cxButton3: TcxButton
      Left = 578
      Top = 548
      Width = 75
      Height = 25
      Cancel = True
      Caption = #1047#1072#1082#1088#1099#1090#1100
      ModalResult = 2
      TabOrder = 5
    end
    object cmbUserState: TcxComboBox
      Left = 40
      Top = 92
      Properties.DropDownListStyle = lsFixedList
      Properties.ReadOnly = True
      Style.HotTrack = False
      TabOrder = 6
      Width = 121
    end
    object clbRoles: TcxCheckListBox
      Left = 24
      Top = 147
      Width = 279
      Height = 375
      Items = <>
      TabOrder = 7
      OnClickCheck = clbRolesClickCheck
    end
    object clbObjects: TcxCheckListBox
      Left = 335
      Top = 147
      Width = 288
      Height = 97
      Items = <>
      TabOrder = 8
      OnClickCheck = clbObjectsClickCheck
    end
    object cxButton4: TcxButton
      Left = 578
      Top = 33
      Width = 75
      Height = 25
      Caption = #1055#1072#1088#1086#1083#1100
      TabOrder = 4
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahCenter
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = labelState
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
      Control = cmbUsers
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = buttonAddUser
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = cxButton3
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroupUserProperties: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = #1057#1074#1086#1081#1089#1090#1074#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      ButtonOptions.Buttons = <>
      Index = 3
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroupUserProperties
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroupUserProperties
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = #1058#1080#1087
      Control = cmbUserState
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = #1056#1086#1083#1080
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = #1044#1086#1089#1090#1091#1087' '#1082' '#1086#1073#1098#1077#1082#1090#1072#1084
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Layout = clTop
      Control = clbRoles
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = clbObjects
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutSplitterItem1: TdxLayoutSplitterItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      Control = cxButton4
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
end
