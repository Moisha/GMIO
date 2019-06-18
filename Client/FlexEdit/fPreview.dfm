object fmPreview: TfmPreview
  Left = 333
  Top = 237
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Preview flex document'
  ClientHeight = 436
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Flex: TFlexPanel
    Left = 0
    Top = 0
    Width = 506
    Height = 383
    HorzScrollBar.Increment = 10
    HorzScrollBar.Range = 320
    HorzScrollBar.Tracking = True
    HorzScrollBar.Visible = False
    VertScrollBar.Increment = 10
    VertScrollBar.Range = 240
    VertScrollBar.Tracking = True
    VertScrollBar.Visible = False
    DocWidth = 320000
    DocHeight = 240000
    DocFrameColor = clBlack
    DocShadowColor = clGray
    DocSpaceBrush.Color = clBtnFace
    DocSpaceBrush.Style = bsBDiagonal
    DocSpaceFill = False
    GridStyle = gsLines
    GridColor = clGray
    GridPixColor = clSilver
    GridHorizSize = 10000
    GridVertSize = 10000
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object pnScriptError: TcxGroupBox
    Left = 0
    Top = 383
    Align = alBottom
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    Visible = False
    Height = 53
    Width = 506
    object mScriptError: TcxMemo
      Left = 0
      Top = 0
      Align = alClient
      Lines.Strings = (
        'mScriptError')
      ParentFont = False
      Style.Font.Charset = RUSSIAN_CHARSET
      Style.Font.Color = clRed
      Style.Font.Height = -13
      Style.Font.Name = 'Courier New'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      TabOrder = 0
      Height = 53
      Width = 506
    end
  end
  object tmrScript: TTimer
    Enabled = False
    OnTimer = tmrScriptTimer
    Left = 352
    Top = 72
  end
end
