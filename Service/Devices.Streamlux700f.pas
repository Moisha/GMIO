unit Devices.Streamlux700f;

interface

uses
  GMGlobals, Devices.ReqCreatorBase, GMConst;

type
  TStreamlux700fReqCreator = class(TDevReqCreator)
  private
    procedure AddChannelRequest(ID_Prm, ID_Src, N_Src: int);
    procedure AddRequestString(const s: string);
  public
    procedure AddRequests(); override;
  end;

implementation

uses
  GMSqlQuery;

{ TStreamlux700fReqCreator }

procedure TStreamlux700fReqCreator.AddRequestString(const s: string);
begin
  AddStringRequestToSendBuf(s + #13, rqtStreamlux700f);
end;

procedure TStreamlux700fReqCreator.AddChannelRequest(ID_Prm, ID_Src, N_Src: int);
begin
  FReqDetails.ID_Prm := ID_Prm;
  case ID_Src of
    SRC_AI:
      case N_Src of
        1: AddRequestString('DQD');
        2: AddRequestString('DQH');
        3: AddRequestString('DQS');
        4: AddRequestString('DV');
        5: AddRequestString('AI1');
        6: AddRequestString('DC');
        7: AddRequestString('DL');
      end;
    SRC_CNT_MTR:
      case N_Src of
        1: AddRequestString('DI+');
      end;
  end;
end;

procedure TStreamlux700fReqCreator.AddRequests;
begin
  ReadFromQueryFmt('select * from DeviceSystem where ID_Device = %d order by ID_Src, N_Src', [FReqDetails.ID_Device],
    procedure(q: TGMSqlQuery)
    begin
      AddChannelRequest(
        q.FieldByName('ID_Prm').AsInteger,
        q.FieldByName('ID_Src').AsInteger,
        q.FieldByName('N_Src').AsInteger);
    end);
end;

end.
