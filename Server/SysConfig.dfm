object SysConfigDlg: TSysConfigDlg
  Left = 611
  Top = 247
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1057#1080#1089#1090#1077#1084#1085#1099#1077' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 241
  ClientWidth = 591
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    591
    241)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 515
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 0
  end
  object Button2: TButton
    Left = 435
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button2Click
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 8
    Width = 588
    Height = 185
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = #1040#1088#1093#1080#1074#1080#1088#1086#1074#1072#1085#1080#1077
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 338
        Height = 13
        Caption = #1047#1072#1076#1072#1081#1090#1077' '#1075#1083#1091#1073#1080#1085#1091' '#1072#1088#1093#1080#1074#1080#1088#1086#1074#1072#1085#1080#1103' ('#1074' '#1076#1085#1103#1093') '#1076#1083#1103' '#1082#1072#1078#1076#1086#1075#1086' '#1090#1080#1087#1072' '#1076#1072#1085#1085#1099#1093
      end
      object leArchVals: TLabeledEdit
        Left = 64
        Top = 40
        Width = 121
        Height = 21
        EditLabel.Width = 45
        EditLabel.Height = 13
        EditLabel.Caption = #1058#1077#1082#1091#1097#1080#1077
        LabelPosition = lpLeft
        LabelSpacing = 6
        TabOrder = 0
      end
      object leArchHours: TLabeledEdit
        Left = 64
        Top = 80
        Width = 121
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = #1063#1072#1089#1086#1074#1099#1077
        LabelPosition = lpLeft
        LabelSpacing = 6
        TabOrder = 1
      end
      object leArchDays: TLabeledEdit
        Left = 64
        Top = 120
        Width = 121
        Height = 21
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = #1057#1091#1090#1086#1095#1085#1099#1077
        LabelPosition = lpLeft
        LabelSpacing = 6
        TabOrder = 2
      end
    end
  end
end
