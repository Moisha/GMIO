object LogReaderMainForm: TLogReaderMainForm
  Left = 0
  Top = 0
  Caption = 'Log Reader'
  ClientHeight = 630
  ClientWidth = 998
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
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 0
    DesignSize = (
      998
      41)
    Height = 41
    Width = 998
    object eFileName: TcxButtonEdit
      Left = 3
      Top = 11
      Anchors = [akLeft, akTop, akRight]
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = cxButtonEdit1PropertiesButtonClick
      TabOrder = 0
      OnKeyDown = eFileNameKeyDown
      Width = 992
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 41
    Width = 998
    Height = 589
    Align = alClient
    TabOrder = 1
    object cxGrid1DBTableView1: TcxGridDBTableView
      OnKeyDown = cxGrid1DBTableView1KeyDown
      Navigator.Buttons.CustomButtons = <>
      OnCellDblClick = cxGrid1DBTableView1CellDblClick
      OnCustomDrawCell = cxGrid1DBTableView1CustomDrawCell
      DataController.DataSource = DataSource1
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnHidingOnGrouping = False
      OptionsCustomize.ColumnMoving = False
      OptionsCustomize.ColumnSorting = False
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsSelection.CellSelect = False
      OptionsSelection.MultiSelect = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object cxGrid1DBTableView1RecId: TcxGridDBColumn
        DataBinding.FieldName = 'RecId'
        Visible = False
      end
      object cxGrid1DBTableView1DT: TcxGridDBColumn
        DataBinding.FieldName = 'DT'
        PropertiesClassName = 'TcxDateEditProperties'
        Properties.DisplayFormat = 'yyyy-mm-dd hh:nn:ss.zzz'
        Properties.EditFormat = 'yyyy-mm-dd hh:nn:ss.zzz'
        Options.Editing = False
        Options.AutoWidthSizable = False
        Options.Sorting = False
        SortIndex = 0
        SortOrder = soAscending
        Width = 141
      end
      object cxGrid1DBTableView1ThreadID: TcxGridDBColumn
        DataBinding.FieldName = 'ThreadID'
        PropertiesClassName = 'TcxLabelProperties'
        Properties.Alignment.Horz = taCenter
        Options.Editing = False
        Options.AutoWidthSizable = False
        Options.Sorting = False
        Width = 53
      end
      object cxGrid1DBTableView1Direction: TcxGridDBColumn
        DataBinding.FieldName = 'Direction'
        PropertiesClassName = 'TcxLabelProperties'
        Properties.Alignment.Horz = taCenter
        Options.Editing = False
        Options.AutoWidthSizable = False
        Options.Sorting = False
        Width = 51
      end
      object cxGrid1DBTableView1Source: TcxGridDBColumn
        DataBinding.FieldName = 'Source'
        Options.Editing = False
        Options.AutoWidthSizable = False
        Options.Sorting = False
        Width = 138
      end
      object cxGrid1DBTableView1Length: TcxGridDBColumn
        DataBinding.FieldName = 'Length'
        PropertiesClassName = 'TcxLabelProperties'
        Properties.Alignment.Horz = taCenter
        Options.Editing = False
        Options.AutoWidthSizable = False
        Options.Sorting = False
      end
      object cxGrid1DBTableView1Text: TcxGridDBColumn
        DataBinding.FieldName = 'Text'
        PropertiesClassName = 'TcxMemoProperties'
        Properties.WordWrap = False
        Options.Editing = False
        Options.Sorting = False
        Width = 556
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  object mdLog: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 472
    Top = 216
    object mdLogDT: TDateTimeField
      FieldName = 'DT'
    end
    object mdLogThreadID: TIntegerField
      FieldName = 'ThreadID'
    end
    object mdLogDirection: TStringField
      FieldName = 'Direction'
    end
    object mdLogSource: TStringField
      FieldName = 'Source'
      Size = 100
    end
    object mdLogLength: TIntegerField
      FieldName = 'Length'
    end
    object mdLogText: TMemoField
      FieldName = 'Text'
      BlobType = ftMemo
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Log Files'
        FileMask = '*.log'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    Options = [fdoAllowMultiSelect, fdoPathMustExist, fdoFileMustExist, fdoDontAddToRecent, fdoForceShowHidden]
    Left = 320
    Top = 128
  end
  object DataSource1: TDataSource
    DataSet = mdLog
    Left = 472
    Top = 152
  end
end
