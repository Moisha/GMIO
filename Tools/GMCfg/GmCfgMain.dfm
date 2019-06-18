object GMCfgMainFrm: TGMCfgMainFrm
  Left = 230
  Top = 153
  Caption = #1050#1086#1085#1092#1080#1075#1091#1088#1072#1090#1086#1088' '#1087#1088#1080#1073#1086#1088#1086#1074' '#1043#1077#1086#1084#1077#1088
  ClientHeight = 446
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lCars: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 446
    Align = alLeft
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnClick = lCarsClick
  end
  object Panel1: TPanel
    Left = 121
    Top = 0
    Width = 702
    Height = 446
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 631
    object mLog: TMemo
      Left = 1
      Top = 97
      Width = 700
      Height = 348
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitLeft = 6
      ExplicitTop = 103
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 700
      Height = 96
      Align = alTop
      TabOrder = 1
      ExplicitWidth = 629
      object Button1: TButton
        Left = 512
        Top = 8
        Width = 75
        Height = 25
        Caption = #1055#1088#1086#1096#1080#1090#1100
        TabOrder = 0
        OnClick = Button1Click
      end
      object eFilename: TEdit
        Left = 72
        Top = 10
        Width = 409
        Height = 21
        TabOrder = 1
      end
      object Button2: TButton
        Left = 484
        Top = 8
        Width = 25
        Height = 25
        Caption = '...'
        TabOrder = 2
        OnClick = Button2Click
      end
      object eStrForSend: TEdit
        Left = 16
        Top = 56
        Width = 361
        Height = 21
        TabOrder = 3
        OnKeyDown = eStrForSendKeyDown
      end
      object Panel3: TPanel
        Left = 16
        Top = 40
        Width = 537
        Height = 3
        TabOrder = 4
      end
      object Button3: TButton
        Left = 384
        Top = 56
        Width = 75
        Height = 25
        Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100
        TabOrder = 5
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 596
        Top = 8
        Width = 25
        Height = 25
        Caption = '?'
        TabOrder = 6
        OnClick = Button4Click
      end
      object eBlock: TEdit
        Left = 16
        Top = 8
        Width = 33
        Height = 21
        TabOrder = 7
      end
      object Button5: TButton
        Left = 464
        Top = 56
        Width = 75
        Height = 25
        Caption = #1042#1089#1077#1084
        TabOrder = 8
        OnClick = Button5Click
      end
      object btUBZ: TButton
        Left = 544
        Top = 56
        Width = 75
        Height = 25
        Caption = 'UBZ'
        TabOrder = 9
        OnClick = btUBZClick
      end
    end
    object Button6: TButton
      Left = 629
      Top = 57
      Width = 63
      Height = 25
      Caption = '485'
      TabOrder = 2
      OnClick = Button6Click
    end
  end
  object ssGM: TServerSocket
    Active = False
    Port = 65500
    ServerType = stThreadBlocking
    OnAccept = ssGMAccept
    OnGetThread = ssGMGetThread
    OnGetSocket = ssGMGetSocket
    OnThreadEnd = ssGMThreadEnd
    OnClientConnect = ssGMClientConnect
    OnClientDisconnect = ssGMClientDisconnect
    OnClientRead = ssGMClientRead
    OnClientError = ssGMClientError
    Left = 16
    Top = 8
  end
  object odProgram: TOpenDialog
    Filter = #1055#1088#1086#1096#1080#1074#1082#1080' (*.flo)|*.flo'
    Left = 497
    Top = 8
  end
end
