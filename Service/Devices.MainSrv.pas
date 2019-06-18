unit Devices.MainSrv;

interface
uses GMGlobals, Windows, Classes, SysUtils, GMConst, Devices.ReqCreatorBase, GMSqlQuery, StdRequest, StdResponce,
     GMGenerics;

type
  TMainSrvReqCreator = class(TDevReqCreator)
  private
    procedure AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
  public
    procedure AddRequests(); override;
  end;

  TMainSrvReqParser = class
    FData: TGMCollection<TValueFromBaseClass>;
  public
    function Parse(ABuf: ArrayOfByte; ALen: int): bool;
    constructor Create();
    destructor Destroy(); override;
    property Data: TGMCollection<TValueFromBaseClass> read FData;
  end;

implementation

uses Math;

{ TMainSrvReqCreator }

procedure TMainSrvReqCreator.AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
var req: IXMLGMIORequestType;
    ut: LongWord;
    src: StdRequest.IXMLSourceType;
begin
  ut := q.FieldByName('Utime').AsInteger;
  ut := Max(ut, NowGM() - UTC_DAY);

  req := NewGMIORequest();
  req.Auth.Login := AUTH_DEFAULT_LOGIN;
  req.Auth.Password := AUTH_DEFAULT_PASSWORD;
  src := req.Data.Sources.Add();
  src.Id := q.FieldByName('MainSrvPrmID').AsInteger;
  src.ValType := DATATYPE_CURRENTS;
  src.ValSrcType := DATACHANNEL_PARAM;
  src.DiagramType := DIAGRAMTYPE_DEFAULT;
  src.AggrType := DATA_AGGREGATION_TYPE_DEFAULT;
  src.UTime1 := ut;
  src.UTime1 := NowGM();

  AddUniversalRequestToSendBuf(req, rqtMainSrv, AUTH_DEFAULT_LOGIN);
end;

procedure TMainSrvReqCreator.AddRequests;
begin
  ReadFromQuery('select * from Params p left outer join CurrVals v on p.ID_Prm = v.ID_Prm where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0',
                AddCurrents_OnePrm);
end;

{ TMainSrvReqParser }

constructor TMainSrvReqParser.Create;
begin
  inherited;
  FData := TGMCollection<TValueFromBaseClass>.Create();
end;

destructor TMainSrvReqParser.Destroy;
begin
  FData.Free();
  inherited;
end;

function TMainSrvReqParser.Parse(ABuf: ArrayOfByte; ALen: int): bool;
var responce: IXMLGMIOResponceType;
    len, i, j: int;
    respXml: string;
    val: TValueFromBaseClass;
begin
  Result := false;
  FData.Clear();
  if ALen < 6 then Exit;

  len := ReadUINT(ABuf, 2);
  if (ABuf[0] <> 255) or (ABuf[1] <> 0) or (ALen < len + 6) then Exit;

  try
    respXml := string(DecompressBufferToString(ABuf, 6, len));
    responce := LoadXMLData_GMIOResponce(respXml);
    if responce = nil then Exit;

    for i := 0 to responce.Data.Count - 1 do
    begin
      if responce.Data[i].ValType = DATATYPE_CURRENTS then
      for j := 0 to responce.Data[i].Count - 1 do
      begin
        val := FData.Add();
        val.ID_Prm := responce.Data[i].Id;
        val.Val.UTime := responce.Data[i].Value[j].Utime;
        val.Val.Val := responce.Data[i].Value[j].Val;
        val.Val.ValType := valueTypeCurrent;
        val.Val.Chn := nil;
      end;

      break; // тут вообще только один должен быть, можно было и по всему списку не крутить цикл
    end;

    Result := FData.Count > 0;
  except
  end;
end;

end.
