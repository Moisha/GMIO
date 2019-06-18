////////////////////////////////////////////
// Служебные функции для приборов ДРК-4ОП
////////////////////////////////////////////
unit Devices.DRK;

interface

uses Devices.ReqCreatorBase, GMConst, GMGlobals;

type
  TDRK4OPReqCreator = class(TDevReqCreator)
  private
    procedure AddRequests_OneChannel(nChannel: int);
  public
    procedure AddRequests(); override;
  end;

implementation

uses ProgramLogFile, SysUtils, GmSqlQuery, Math;

{ TDRK4OPReqCreator }

procedure TDRK4OPReqCreator.AddRequests_OneChannel(nChannel: int);
var q: TGMSqlQuery;
    buf: ArrayOfByte;
begin
  FReqDetails.ClearReqLink();
  FReqDetails.AddReqLink(0);
  FReqDetails.AddReqLink(0);

  q := TGMSqlQuery.Create();
  q.SQL.Text := 'select * from DeviceSystem where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and N_Src = ' + IntToStr(nChannel);

  try
    q.Open();
    while not q.Eof do
    begin
      case q.FieldByName('ID_Src').AsInteger of
        SRC_AI: FReqDetails.ReqLinks[0].id := q.FieldByName('ID_Prm').AsInteger;
        SRC_CNT_MTR: FReqDetails.ReqLinks[1].id := q.FieldByName('ID_Prm').AsInteger;
      end;
      q.Next();
    end;

    buf := TextNumbersStringToArray('D0 D0 D0 D0 D1 ' + IntToHex(FReqDetails.DevNumber, 2) + ' D2 C' + IntToStr(nChannel));
    AddBufRequestToSendBuf(buf, Length(buf), rqtDRK);
  except
    on e: Exception do
      ProgramLog().AddException('AddI7041DToSendBuf - ' + e.Message);
  end;
  q.Free();
end;

procedure TDRK4OPReqCreator.AddRequests;
begin
  AddRequests_OneChannel(1);
  AddRequests_OneChannel(2);
end;

end.
