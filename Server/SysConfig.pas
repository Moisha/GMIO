unit SysConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GMSQLQuery, StdCtrls, ComCtrls, ExtCtrls, GMGlobals;

type
  TSysConfigDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    leArchVals: TLabeledEdit;
    leArchHours: TLabeledEdit;
    leArchDays: TLabeledEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    q: TGMSqlQuery;
    function CheckLEInt(le: TLabeledEdit): bool;
    procedure ReadConfig();
    procedure WriteConfig();
    procedure WriteConfigVal(key, val: string);
  public
    { Public declarations }
    class function Dlg(): TSysConfigDlg;
    function ShowModal(): TModalResult; reintroduce; 
  end;

implementation

{$R *.dfm}

{ TSysConfigDlg }

var
  SysConfigDlg: TSysConfigDlg;

class function TSysConfigDlg.Dlg: TSysConfigDlg;
begin
  if SysConfigDlg = nil then
    Application.CreateForm(TSysConfigDlg, SysConfigDlg);

  Result := SysConfigDlg;
end;

function TSysConfigDlg.ShowModal: TModalResult;
begin
  ReadConfig();
  Result := inherited ShowModal();
  if Result = mrOk then
    WriteConfig();
end;

procedure TSysConfigDlg.FormCreate(Sender: TObject);
begin
  q := TGMSqlQuery.Create();
end;

procedure TSysConfigDlg.FormDestroy(Sender: TObject);
begin
  q.Free();
end;

function TSysConfigDlg.CheckLEInt(le: TLabeledEdit): bool;
var c, n: int;
    p: TControl;
begin
  if Trim(le.Text) <> '' then
  begin
    Val(Trim(le.Text), n, c);
    Result := c = 0;
    if not Result then
    begin
      p := le.Parent;
      while (p <> nil) and not (p is TTabSheet) do
        p := p.Parent;

      if p <> nil then
        TTabSheet(p).PageControl.ActivePage := TTabSheet(p);

      Application.ProcessMessages();

      BlinkControl(le);
    end
    else
    begin
      le.Text := IntToStr(n);
    end;
  end
  else
  begin
    Result := true;
  end;
end;

procedure TSysConfigDlg.Button2Click(Sender: TObject);
begin
  if CheckLEInt(leArchVals) and CheckLEInt(leArchHours) and CheckLEInt(leArchDays) then
    ModalResult := mrOk;
end;

procedure TSysConfigDlg.ReadConfig;
begin
  leArchVals.Text := '';
  leArchHours.Text := '';
  leArchDays.Text := '';

  q.Close();
  q.SQL.Text := 'select * from SysConfig';
  q.Open();
  while not q.Eof do
  begin
    if q.FieldByName('PrmName').AsString = 'StoreVals_Days' then
      leArchVals.Text := q.FieldByName('PrmValue').AsString;

    if q.FieldByName('PrmName').AsString = 'StoreArchHour_Days' then
      leArchHours.Text := q.FieldByName('PrmValue').AsString;

    if q.FieldByName('PrmName').AsString = 'StoreArchDay_Days' then
      leArchDays.Text := q.FieldByName('PrmValue').AsString;

    q.Next();
  end;
end;

procedure TSysConfigDlg.WriteConfigVal(key, val: string);
begin
  q.Execute('do $$ begin perform SetConfigPrm(' + QuotedStr(key) + ', ' + QuotedStr(val) + '); end $$;');
end;

procedure TSysConfigDlg.WriteConfig;
begin
  WriteConfigVal('StoreVals_Days', leArchVals.Text);
  WriteConfigVal('StoreArchHour_Days', leArchHours.Text);
  WriteConfigVal('StoreArchDay_Days', leArchDays.Text);
end;

end.
