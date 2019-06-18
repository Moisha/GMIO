unit Devices.Isco4250;

interface

uses Windows, GMGlobals, Classes, SysUtils, Devices.ReqCreatorBase, GMConst;

type
  TIsco4250ReqCreator = class(TDevReqCreator)
  public
    procedure AddRequests(); override;
  end;

implementation

{ TIsco4250ReqCreator }

procedure TIsco4250ReqCreator.AddRequests;
begin
  // перевод в режим текстового терминала
  AddStringRequestToSendBuf('?????????????'#13, rqtDummy);
  // запрос данных всем колхозом
  AddStringRequestToSendBuf('DATA'#13, rqtIsco4250);
end;

end.
