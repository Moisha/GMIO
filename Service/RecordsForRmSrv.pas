////////////////////////////////////////////
// Классы для опроса удаленного сервера
////////////////////////////////////////////
unit RecordsForRmSrv;

interface

uses Windows, Classes, SysUtils, Math, GMSqlQuery, GMGlobals, DateUtils;

type
  TRemoteSrvRecord = class(TCollectionItem)
  private
    procedure CreateRequest(var bufs: TTwoBuffers);
  public
    ID_Prm: int;
    sRemoteSrv: string;
    RemotePort: int;

    ID_Obj, ID_Device: int;

    N_Car,
    ID_DevType, DevNumber,
    ID_Src, N_Src: int;

    bRenewed: bool;
    uDTLast: LongWord;

    constructor Create(Collection: TCollection); override;
  end;

  TRemoteSrvRecords = class (TCollection)
  private
    qLst, qDT, qW: TGMSqlQuery;
    iCurrentID: int;
    tLastRead: TDateTime;

    function GetRec(i: int): TRemoteSrvRecord;
    procedure ReadInitialDT(r: TRemoteSrvRecord);
    procedure AddRmSrvRecord;
    procedure ClearUnused;
    procedure ResetRenewed();
    function AddUnique(ID_Prm: int): TRemoteSrvRecord;
  public
    procedure RequestNext(var bufs: TTwoBuffers);
    procedure ReadRemoteRecords();
    constructor Create();
    destructor Destroy; override;
    property Rec[i: int]: TRemoteSrvRecord read GetRec; default;
    function RecByParams(N_Car, DevType, NDevice, PrmType, NPrm: int): TRemoteSrvRecord;
  end;

implementation

uses GMConst, ProgramLogFile;

const
    SQLConst_SelectParamDevObj =
      'select * from Params p join Devices d on p.ID_Device = d.ID_Device ' +
                                 ' join Objects o on o.ID_Obj = d.ID_Obj ' +
      ' where p.ID_PT > 0 ';


{ TRemoteSrvRecord }

constructor TRemoteSrvRecord.Create(Collection: TCollection);
begin
  inherited;

  bRenewed := true;
  uDTLast := 0;
end;

procedure TRemoteSrvRecord.CreateRequest(var bufs: TTwoBuffers);
begin
  bufs.BufSend[0] := 231;
  bufs.BufSend[1] := 180;

  bufs.LengthSend := WriteUINT(bufs.BufSend, 2, N_Car);
  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, ID_DevType);
  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, DevNumber);
  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, ID_Src);
  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, N_Src);
  bufs.LengthSend := WriteByte(bufs.BufSend, bufs.LengthSend, 0);

  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, uDTLast);
  bufs.LengthSend := WriteUINT(bufs.BufSend, bufs.LengthSend, uDTLast + 3600 * 12);
end;

{ TRemoteSrvRecords }

function TRemoteSrvRecords.AddUnique(ID_Prm: int): TRemoteSrvRecord;
var i: int;
begin
  for i := 0 to Count - 1 do
    if Rec[i].ID_Prm = ID_Prm then
    begin
      Result := Rec[i];
      Result.bRenewed := true;
      Exit;
    end;

  Result := TRemoteSrvRecord(Add());
  Result.ID_Prm := ID_Prm;
end;

procedure TRemoteSrvRecords.AddRmSrvRecord();
var r: TRemoteSrvRecord;
begin
  r := AddUnique(qLst.FieldByName('ID_Prm').AsInteger);

  r.sRemoteSrv := qLst.FieldByName('RemoteSrv').AsString;
  r.RemotePort := qLst.FieldByName('RemotePort').AsInteger;
  r.ID_Obj := qLst.FieldByName('ID_Obj').AsInteger;
  r.ID_Device := qLst.FieldByName('ID_Device').AsInteger;
  r.N_Car := qLst.FieldByName('N_Car').AsInteger;
  r.ID_DevType := qLst.FieldByName('ID_DevType').AsInteger;
  r.DevNumber := qLst.FieldByName('Number').AsInteger;
  r.ID_Src := qLst.FieldByName('ID_Src').AsInteger;
  r.N_Src := qLst.FieldByName('N_Src').AsInteger;

  ReadInitialDT(r);
end;

procedure TRemoteSrvRecords.ClearUnused();
var i: int;
begin
  for i := Count - 1 downto 0 do
    if not Rec[i].bRenewed then
      Delete(i);
end;

constructor TRemoteSrvRecords.Create();
begin
  inherited Create(TRemoteSrvRecord);

  qLst := TGMSqlQuery.Create();
  qDT := TGMSqlQuery.Create();
  qW := TGMSqlQuery.Create();

  iCurrentID := 0;
  tLastRead := 0; 
end;

destructor TRemoteSrvRecords.Destroy;
begin
  qLst.Free();
  qDT.Free();
  qW.Free();

  inherited;
end;

function TRemoteSrvRecords.GetRec(i: int): TRemoteSrvRecord;
begin
  Result := TRemoteSrvRecord(Items[i]);
end;

procedure TRemoteSrvRecords.ResetRenewed;
var i: int;
begin
  for i := 0 to Count - 1 do
    Rec[i].bRenewed := false;
end;

procedure TRemoteSrvRecords.ReadInitialDT(r: TRemoteSrvRecord);
begin
  if r.uDTLast > 0 then Exit;

  try
    qDT.Close();
    qDT.SQL.Text := ' select UTime from Vals ' +
                    '  where ID_Prm = ' + IntToStr(r.ID_Prm) +
                    ' order by UTime desc limit 1';
    qDT.Open();

    if not qDT.Eof then
      r.uDTLast := qDT.FieldByName('UTime').AsInteger;
  except
    on e: Exception do
      ProgramLog().AddException('TGMRemoteSrvDataThread.qDT - ' + e.Message);
  end;
  qDT.Close();
end;

procedure TRemoteSrvRecords.ReadRemoteRecords();
begin
  ResetRenewed();

  try
    qLst.Close();
    qLst.SQL.Text := SQLConst_SelectParamDevObj +
                  '   and o.ObjType = ' + IntToStr(OBJ_TYPE_REMOTE_SRV) +
                  ' order by o.ID_Obj';
    qLst.Open();

    while not qLst.Eof do
    begin
      AddRmSrvRecord();
      qLst.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('TGMRemoteSrvDataThread.qLst - ' + e.Message);
  end;

  tLastRead := Now();
  qLst.Close();

  ClearUnused();
end;

procedure TRemoteSrvRecords.RequestNext(var bufs: TTwoBuffers);
var i: int;

  function DoRequest(i: int): bool;
  begin
    ProgramLog().AddMessage('TRemoteSrvRecords.RequestNext - DoRequest');
    Result := NowGM() - UINT(IfThen(NowIsSummer(), 3600, 0)) - Rec[i].uDTLast >= 2 * 60;

    if Result then
    begin
      ProgramLog().AddMessage('TRemoteSrvRecords.RequestNext - CreateRequest');
      Rec[i].CreateRequest(bufs);
      iCurrentID := i + 1;
    end;
  end;

begin
  if Abs(Now() - tLastRead) > 60 * OneSecond then
    ReadRemoteRecords();

  if Count = 0 then Exit;

  if iCurrentID >= Count then
    iCurrentID := 0;

  bufs.LengthSend := 0;
  for i := iCurrentID to Count - 1 do
    if DoRequest(i) then break;

  // дошли до конца, ничего не нашли, попробуем сначала
  if bufs.LengthSend = 0 then
  begin
    for i := 0 to Count - 1 do
      if DoRequest(i) then break;
  end;
end;

function TRemoteSrvRecords.RecByParams(N_Car, DevType, NDevice, PrmType, NPrm: int): TRemoteSrvRecord;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Rec[i].N_Car = N_Car)
       and (Rec[i].ID_DevType = DevType)
       and (Rec[i].DevNumber = NDevice)
       and (Rec[i].ID_Src = PrmType)
       and (Rec[i].N_Src = NPrm) then
    begin
      Result := Rec[i];
      break;
    end;
  end;
end;

end.
