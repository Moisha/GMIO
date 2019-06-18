////////////////////////////////////////////
// Служебные функции для приборов ICP
////////////////////////////////////////////
unit Devices.ICP;

interface

uses GMGlobals, Devices.ReqCreatorBase, GMConst;

type
  TICP7017ReqCreator = class(TDevReqCreator)
  protected
    function GetReqType(): T485RequestType; virtual;
  public
    procedure AddRequests(); override;
  end;

  TICP7041DReqCreator = class(TDevReqCreator)
  public
    procedure AddRequests(); override;
  end;

implementation

uses ProgramLogFile, SysUtils, GmSqlQuery, Math;

{ TDevReqCreator }

procedure TICP7017ReqCreator.AddRequests;
begin
  AddStringRequestToSendBuf('#' + IntToHex(FReqDetails.DevNumber and $FF, 2) + #13, GetReqType());
end;

function TICP7017ReqCreator.GetReqType: T485RequestType;
begin
  Result := rqtICP7017;
end;

{ TICP7041DReqCreator }

procedure TICP7041DReqCreator.AddRequests;
var q: TGMSqlQuery;
    N_Src: int;
    rqtp: T485RequestType;
begin
  q := TGMSqlQuery.Create();
  q.SQL.Text := 'select count(*) from Params where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 and ID_Src = ' + IntToStr(SRC_DI);

  try
    q.Open();
    if not q.Eof and (q.Fields[0].AsInteger > 0) then // дискреты нужны
      AddStringRequestToSendBuf('@' + IntToHex(FReqDetails.DevNumber and $FF, 2) + #13, rqtICP7041D_ALLDI);
  except
    on e: Exception do
      ProgramLog().AddException('AddI7041DToSendBuf - ' + e.Message);
  end;

  // накопительные
  q.Close();
  q.SQL.Text := 'select * from Params where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 and ID_Src = ' + IntToStr(SRC_CNT_DI) + ' order by N_Src';
  try
    q.Open();
    while not q.Eof do
    begin
      N_Src := q.FieldByName('N_Src').AsInteger;
      if N_Src in [0..13] then
      begin
        rqtp := T485RequestType(ord(rqtICP7041D_CNT0) + N_Src);
        AddStringRequestToSendBuf('#' + IntToHex(FReqDetails.DevNumber and $FF, 2) + IntToHex(N_Src, 1) + #13, rqtp);
      end;

      q.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('AddI7041DToSendBuf2 - ' + e.Message);
  end;
  q.Free();
end;

end.
