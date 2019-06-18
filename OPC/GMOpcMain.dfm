object GMOpcMainForm: TGMOpcMainForm
  Left = 901
  Top = 315
  Caption = 'Sirius ATM. OPC-'#1089#1077#1088#1074#1077#1088
  ClientHeight = 400
  ClientWidth = 563
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 540
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      563
      41)
    object lState: TLabel
      Left = 8
      Top = 16
      Width = 27
      Height = 13
      Caption = 'lState'
    end
    object btnOpts: TButton
      Left = 476
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #1057#1074#1103#1079#1100
      TabOrder = 0
      OnClick = btnOptsClick
    end
  end
  object vstOpc: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 563
    Height = 359
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoShowSortGlyphs, hoVisible]
    NodeDataSize = 0
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnGetText = vstOpcGetText
    OnPaintText = vstOpcPaintText
    Columns = <
      item
        Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 263
        WideText = #1058#1101#1075
      end
      item
        Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 150
        WideText = #1044#1072#1090#1072
      end
      item
        Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 2
        Width = 150
        WideText = #1047#1085#1072#1095#1077#1085#1080#1077
      end>
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 352
    Top = 16
  end
end
