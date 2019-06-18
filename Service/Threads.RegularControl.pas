unit Threads.RegularControl;

interface

uses Windows, Classes, SysUtils, GMGenerics, Threads.Base, GMSqlQuery, GMGlobals, Math;

type
  TRegularControlledChannel = class
    ID_Prm, SourcePrmID: int;
    LastControlledDT: LongWord;
    bVerified: bool;
  end;

  TRegularControl = class(TGMThread)
  private
    FChnList: TGMCollection<TRegularControlledChannel>;
    procedure ProcessChannelList;
    procedure UpdateRemoteChannelList;
    procedure UpdateChannel(q: TGMSqlQuery; obj: pointer);
    procedure ProcessChannel(chn: TRegularControlledChannel);
    procedure SendControlSignal(chn: TRegularControlledChannel; utime: LongWord; val: double);
  protected
    procedure SafeExecute; override;
    procedure Process;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TRegularControl }

uses RequestThreadsContainer;

procedure TRegularControl.UpdateChannel(q: TGMSqlQuery; obj: pointer);
var chn, selChn: TRegularControlledChannel;
begin
  selChn := nil;
  for chn in FChnList do
    if chn.ID_Prm = q.FieldByName('ID_Prm').AsInteger then
    begin
      selChn := chn;
      break;
    end;

  if selChn = nil then
  begin
    selChn := FChnList.Add();
    selChn.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
    selChn.SourcePrmID := q.FieldByName('SourcePrmID').AsInteger;
    selChn.LastControlledDT := 0;
  end
  else
  begin
    if selChn.SourcePrmID <> q.FieldByName('SourcePrmID').AsInteger then
    begin
      selChn.SourcePrmID := q.FieldByName('SourcePrmID').AsInteger;
      selChn.LastControlledDT := 0;
    end;
  end;

  selChn.bVerified := true;
end;

procedure TRegularControl.UpdateRemoteChannelList();
var i: int;
begin
  for i := 0 to FChnList.Count - 1 do
    FChnList[i].bVerified := false;

  ReadFromQuery('select * from Params where coalesce(SourcePrmID, 0) > 0', UpdateChannel);

  for i := FChnList.Count - 1 downto 0 do
    if not FChnList[i].bVerified then
      FChnList.Delete(i);
end;

procedure TRegularControl.AfterConstruction;
begin
  FChnList := TGMCollection<TRegularControlledChannel>.Create();
  inherited;
end;

procedure TRegularControl.BeforeDestruction;
begin
  inherited;
  FChnList.Free();
end;

procedure TRegularControl.SendControlSignal(chn: TRegularControlledChannel; utime: LongWord; val: double);
var age: int;
begin
  age := NowGM() - utime;
  age := Max(Min(age, $FFFF), 0);
  ThreadsContainer.SetChannelValue(chn.ID_Prm, val, 0, age);
  chn.LastControlledDT := utime;
end;

procedure TRegularControl.ProcessChannel(chn: TRegularControlledChannel);
var utime: LongWord;
    val: double;
    res: ArrayOfString;
begin
  res := QueryResultArray_FirstRow('select Val, UTime from CurrVals where ID_Prm = ' + IntToStr(chn.SourcePrmID));
  if Length(res) < 2 then Exit;

  val := MyStrToFloatDef(res[0], 0);
  utime := StrToIntDef(res[1], 0);
  if utime <= chn.LastControlledDT then Exit;

  SendControlSignal(chn, utime, val);
end;

procedure TRegularControl.ProcessChannelList();
var chn: TRegularControlledChannel;
begin
  for chn in FChnList do
    ProcessChannel(chn);
end;

procedure TRegularControl.Process();
begin
  UpdateRemoteChannelList();
  ProcessChannelList();
end;

procedure TRegularControl.SafeExecute;
var t: int64;
begin
  while not Terminated do
  begin
    t := GetTickCount();
    Process();
    SleepThread(Max(100, COMDevicesRequestInterval * 1000 - Abs(t - GetTickCount())));
  end;
end;

end.
