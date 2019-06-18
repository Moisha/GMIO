object XmlPreviewDlg: TXmlPreviewDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'XmlPreviewDlg'
  ClientHeight = 690
  ClientWidth = 853
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object cxMemo1: TcxMemo
    Left = 0
    Top = 0
    Align = alClient
    Lines.Strings = (
      'cxMemo1')
    ParentFont = False
    Properties.ScrollBars = ssBoth
    Properties.WordWrap = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -16
    Style.Font.Name = 'Courier New'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    TabOrder = 0
    OnKeyDown = cxMemo1KeyDown
    Height = 690
    Width = 853
  end
end
