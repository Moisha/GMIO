unit SimulatorMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IniFiles, AppConfigFile, ProgramLogFile, GMGlobals,
  ConnParamsStorage, ActiveX, GmSqlQuery, CheckConnStr, Vcl.StdCtrls, Vcl.ExtCtrls, Generics.Collections,
  Math, GMConst, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    leLastDT: TLabeledEdit;
    Button1: TButton;
    eInitialHours: TEdit;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    q: TGMSqlQuery;
    lstChn: TList<int>;
    udtDataStart: ULong;
    function ReadAndCheckSQLParams: bool;
    function ReadINI: bool;
    function MainConnectSQL(Params: TZConnectionParams): bool;
    procedure InitialWriteData(hours: int);
    procedure ReadChnList;
    procedure AddChn(q: TGMSqlQuery; obj: pointer);
    function OneChnWriteSql(ID_Prm: int; udt: LongWord): string;
    function AllChnsChnWriteSql(udt: LongWord): string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddChn(q: TGMSqlQuery; obj: pointer);
begin
  lstChn.Add(q.FieldByName('ID_Prm').AsInteger);
end;

procedure TForm1.ReadChnList();
begin
  lstChn.Clear();
  ReadFromQuery('select ID_Prm from Params where ID_Src = ' + IntToStr(SRC_AI) + ' order by ID_Prm', nil, AddChn);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  q.Free();
  lstChn.Free();
end;

function TForm1.OneChnWriteSql(ID_Prm: int; udt: LongWord): string;
begin
  Result := Format('perform WriteVal(%d, %d, %d);'#13#10, [ID_Prm, udt, (udt - udtDataStart) div UTC_MINUTE mod ID_Prm]);
end;

function TForm1.AllChnsChnWriteSql(udt: LongWord): string;
var i: int;
begin
  Result := '';
  for i := 0 to lstChn.Count - 1 do
    Result := Result + OneChnWriteSql(lstChn[i], udt);
end;

procedure TForm1.InitialWriteData(hours: int);
var
    udt: LongWord;
    sql: string;
begin
  udtDataStart := NowGM() - hours * UTC_HOUR;
  udt := udtDataStart;

  while udt < NowGM() do
  begin
    sql := AllChnsChnWriteSql(udt);
    ExecPLSQL(sql);
    udt := udt + UTC_MINUTE;

    ProgressBar1.Position := Floor((udt - udtDataStart) / (NowGM() - udtDataStart) * 100);
    Form1.Refresh();
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  InitialWriteData(StrToInt(eInitialHours.Text));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetMainConfigFileClass(TGMServerMainConfigFile);
  q := TGMSqlQuery.Create();
  lstChn := TList<int>.Create();

  if not ReadINI() then
  begin
    Application.Terminate();
    Exit;
  end;

  ReadChnList();
  udtDataStart := LocalToUTC(Now());
end;

function TForm1.MainConnectSQL(Params: TZConnectionParams): bool;
begin
  Result := false;
  try
    CheckConnStrDlg := TCheckConnStrDlg.Create(Application);
    Result := CheckConnStrDlg.CheckConnectionString(Params);
  except
    on e: Exception do
      ShowMessageBox('Ошибка соединения с SQL: ' + e.Message);
  end;
end;

function TForm1.ReadAndCheckSQLParams(): bool;
var Params: TZConnectionParams;
begin
  Result := GMMainConfigFile.ReadSQLConnectionParams(Params);
  if not Result then Exit;

  Result := MainConnectSQL(Params);
  if not Result then Exit;

  SetGlobalSQLConnectionParams(Params);
end;

function TForm1.ReadINI: bool;
begin
  Result := (GMMainConfigFile.CheckMainINIFile() = iftINI) and ReadAndCheckSQLParams();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ExecPLSQL(AllChnsChnWriteSql(NowGM()));
  leLastDT.Text := DateTimeToStr(Now());
end;

end.
