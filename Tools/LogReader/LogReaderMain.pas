unit LogReaderMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, Data.DB, cxDBData, cxTextEdit,
  cxMaskEdit, cxButtonEdit, dxmdaset, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxGroupBox, GMGlobals, cxMemo, cxCalendar, cxLabel;

type
  TLogReaderMainForm = class(TForm)
    cxGroupBox1: TcxGroupBox;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    mdLog: TdxMemData;
    mdLogText: TMemoField;
    mdLogDT: TDateTimeField;
    mdLogThreadID: TIntegerField;
    mdLogDirection: TStringField;
    mdLogSource: TStringField;
    mdLogLength: TIntegerField;
    eFileName: TcxButtonEdit;
    FileOpenDialog1: TFileOpenDialog;
    DataSource1: TDataSource;
    cxGrid1DBTableView1RecId: TcxGridDBColumn;
    cxGrid1DBTableView1DT: TcxGridDBColumn;
    cxGrid1DBTableView1ThreadID: TcxGridDBColumn;
    cxGrid1DBTableView1Direction: TcxGridDBColumn;
    cxGrid1DBTableView1Source: TcxGridDBColumn;
    cxGrid1DBTableView1Length: TcxGridDBColumn;
    cxGrid1DBTableView1Text: TcxGridDBColumn;
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxGrid1DBTableView1CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure eFileNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxGrid1DBTableView1CellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cxGrid1DBTableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FSearchString: string;
    procedure ProcessFiles;
    procedure InitGrid;
    procedure ProcessFile(const fileName: string);
    procedure ProcessString(const s: string);
    procedure PostDataSet;
    procedure SearchNext;
    procedure AskForSearchString;
    procedure PreviewXml(const s: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogReaderMainForm: TLogReaderMainForm;

implementation

uses Dateutils, XmlPreview;

{$R *.dfm}

function SQLStringToDateTime(const str: string): TDateTime;
var
  y, m, d, hh, nn, ss, zz: word;
  s: string;
begin
  Result := 0;
  try
    s := Trim(str);
    if pos('-', s) > 0 then
    begin
      y := StrToInt(Copy(s, 1, 4));
      m := StrToInt(Copy(s, 6, 2));
      d := StrToInt(Copy(s, 9, 2));

      Delete(s, 1, 11);
      Result := EncodeDate(y, m, d);
    end;

    if Length(s) > 0 then
    begin
      hh := StrToInt(Copy(s, 1, 2));
      nn := StrToInt(Copy(s, 4, 2));

      ss := 0;
      if Length(s) > 6 then
        ss := StrToInt(Copy(s, 7, 2));

      zz := 0;
      if Length(s) > 9 then
        zz := StrToInt(Copy(s, 10, 3));

      Result := Result + EncodeTime(hh, nn, ss, zz);
    end;
  except
    Result := 0;
  end;
end;

procedure TLogReaderMainForm.PreviewXml(const s: string);
begin
  XmlPreviewDlg.SetString(s);
  XmlPreviewDlg.Show();
end;

procedure TLogReaderMainForm.cxGrid1DBTableView1CellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  PreviewXml(ACellViewInfo.RecordViewInfo.GridRecord.Values[cxGrid1DBTableView1Text.Index]);
end;

procedure TLogReaderMainForm.cxGrid1DBTableView1CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);

  procedure ApplyColor(color: TColor);
  begin
    if not AViewInfo.Selected or AViewInfo.Focused then
      ACanvas.Font.Color := color;
  end;

var dir: string;
begin
  dir := Trim(AViewInfo.RecordViewInfo.GridRecord.Values[3]);

  if dir = '<' then
    ApplyColor(clRed);

  if dir = '?' then
    ApplyColor(clBlue);
end;

procedure TLogReaderMainForm.cxGrid1DBTableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('F')) then
  begin
    AskForSearchString();
    SearchNext();
  end;

  if (Shift = []) and (Key = VK_F3) then
    SearchNext();

  if (Shift = []) and (Key = VK_RETURN) then
    PreviewXml(cxGrid1DBTableView1.DataController.Values[cxGrid1DBTableView1.DataController.FocusedRecordIndex, cxGrid1DBTableView1Text.Index]);
end;

procedure TLogReaderMainForm.AskForSearchString();
begin
  InputQuery('Строка поиска', '', FSearchString);
end;

procedure TLogReaderMainForm.SearchNext();
var
  recordIndex, i: int;
  s: string;
begin
  for i := cxGrid1DBTableView1.DataController.FocusedRowIndex + 1 to cxGrid1DBTableView1.DataController.RowCount - 1 do
  begin
    recordIndex := cxGrid1DBTableView1.DataController.DataControllerInfo.GetRowInfo(i).RecordIndex;
    s := cxGrid1DBTableView1.DataController.Values[recordIndex, cxGrid1DBTableView1Text.Index];

    if Pos(FSearchString, s) > 0 then
    begin
      cxGrid1DBTableView1.DataController.FocusedRowIndex := i;
      cxGrid1DBTableView1.DataController.ClearSelection();
      cxGrid1DBTableView1.DataController.SelectRows(i, i);
      Exit;
    end;
  end;
end;

procedure TLogReaderMainForm.eFileNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ProcessFiles();
    Key := 0;
  end;
end;

procedure TLogReaderMainForm.FormCreate(Sender: TObject);
begin
{$ifdef DEBUG}
  eFileName.Text := 'D:\Programs\Delphi\Geomer\GMIOPSvc_trace1.log';
{$endif}
end;

procedure TLogReaderMainForm.FormShow(Sender: TObject);
var i: int;
begin
  eFileName.Text := '';
  for i := 1 to ParamCount() do
  begin
    if eFileName.Text <> '' then
      eFileName.Text := eFileName.Text + ', ';

    eFileName.Text := eFileName.Text + Paramstr(i);
  end;

  ProcessFiles();
end;

procedure TLogReaderMainForm.InitGrid();
begin
  mdLog.DisableControls();
  mdLog.Close();
  mdLog.Open();
end;

procedure TLogReaderMainForm.PostDataSet();
begin
  mdLog.EnableControls();
end;

procedure TLogReaderMainForm.ProcessString(const s: string);
var
  lst: TArray<string>;
  dt: TDateTime;
  n: int;
begin
  if Trim(s) = '' then
    Exit;

  lst := s.Split([#9]);
  if Length(lst) < 2 then
    Exit;

  dt := SQLStringToDateTime(lst[0]);
  if CompareDateTime(dt, 0) <= 0 then Exit;

  mdLog.Append();
  mdLog.FieldByName('DT').AsDateTime := dt;

  if Length(lst) > 1 then
    mdLog.FieldByName('Text').AsString := lst[High(lst)];

  if Length(lst) > 2 then
  begin
    n := StrToIntDef(lst[1], -1);
    if n > 0 then
      mdLog.FieldByName('ThreadID').AsInteger := n;
  end;

  if Length(lst) > 3 then
    mdLog.FieldByName('Direction').AsString := lst[2];

  if Length(lst) > 4 then
    mdLog.FieldByName('Source').AsString := lst[3];

  if Length(lst) > 5 then
  begin
    n := StrToIntDef(lst[4], -1);
    if n > 0 then
      mdLog.FieldByName('Length').AsInteger := n;
  end;
  mdLog.Post();
end;

procedure TLogReaderMainForm.ProcessFile(const fileName: string);
var
  f: TSTringList;
  i: int;
begin
  if not FileExists(fileName) then
    Exit;
  f := TSTringList.Create();
  try
    f.LoadFromFile(fileName);
    for i := 0 to f.Count - 1 do
      ProcessString(f[i]);
  finally
    f.Free();
  end;
end;

procedure TLogReaderMainForm.ProcessFiles();
var
  sl: TSTringList;
  i: int;
begin
  InitGrid();

  sl := TSTringList.Create();
  try
    sl.CommaText := eFileName.Text;
    for i := 0 to sl.Count - 1 do
    begin
      ProcessFile(sl[i]);
    end;
  finally
    sl.Free();
    PostDataSet();
  end;
end;

procedure TLogReaderMainForm.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if not FileOpenDialog1.Execute() then
    Exit;

  eFileName.Text := FileOpenDialog1.Files.CommaText;
  ProcessFiles();
end;

end.
