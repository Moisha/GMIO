object UserDlg: TUserDlg
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
  ClientHeight = 182
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 347
    Height = 182
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DataModuleSkin.dxLayoutSkinLookAndFeel1
    object cxButton1: TcxButton
      Left = 178
      Top = 145
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 4
      OnClick = cxButton1Click
    end
    object cxButton2: TcxButton
      Left = 260
      Top = 145
      Width = 75
      Height = 25
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 5
    end
    object eLogin: TcxTextEdit
      Left = 105
      Top = 12
      Style.HotTrack = False
      TabOrder = 0
      Text = 'eLogin'
      Width = 121
    end
    object ePassword: TcxTextEdit
      Left = 105
      Top = 72
      Properties.EchoMode = eemPassword
      Properties.PasswordChar = '*'
      Style.HotTrack = False
      TabOrder = 2
      Text = 'ePassword'
      Width = 121
    end
    object ePassword2: TcxTextEdit
      Left = 105
      Top = 102
      Properties.EchoMode = eemPassword
      Properties.PasswordChar = '*'
      Style.HotTrack = False
      TabOrder = 3
      Text = 'ePassword2'
      Width = 121
    end
    object cmbUserState: TcxComboBox
      Left = 105
      Top = 42
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 1
      Width = 121
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = cxButton1
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = #1051#1086#1075#1080#1085
      Control = eLogin
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = #1055#1072#1088#1086#1083#1100
      Control = ePassword
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = #1055#1086#1076#1090#1074#1077#1088#1078#1076#1077#1085#1080#1077
      Control = ePassword2
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = #1058#1080#1087
      Control = cmbUserState
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
end
