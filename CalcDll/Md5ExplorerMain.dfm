object Form8: TForm8
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1040#1085#1072#1083#1080#1079#1072#1090#1086#1088' MD5'
  ClientHeight = 300
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxButtonEdit1: TcxButtonEdit
    Left = 62
    Top = 8
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = cxButtonEdit1PropertiesButtonClick
    TabOrder = 0
    Width = 563
  end
  object cxLabel1: TcxLabel
    Left = 9
    Top = 9
    Caption = #1060#1072#1081#1083
  end
  object cxButton1: TcxButton
    Left = 462
    Top = 35
    Width = 163
    Height = 25
    Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100
    TabOrder = 2
    OnClick = cxButton1Click
  end
  object cxMemo1: TcxMemo
    Left = 8
    Top = 65
    Properties.ReadOnly = True
    TabOrder = 3
    Height = 227
    Width = 617
  end
  object OpenDialog1: TOpenDialog
    Filter = #1041#1080#1073#1083#1080#1086#1090#1077#1082#1080' dll|*.dll|'#1042#1089#1077' '#1092#1072#1081#1083#1099'|*.*'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = #1042#1099#1073#1077#1088#1080#1090#1077' '#1073#1080#1073#1083#1080#1086#1090#1077#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1074#1077#1088#1082#1080
    Left = 304
    Top = 152
  end
  object dxSkinController1: TdxSkinController
    Kind = lfStandard
    NativeStyle = False
    SkinName = 'Office2010Silver'
    Left = 248
    Top = 24
  end
end
