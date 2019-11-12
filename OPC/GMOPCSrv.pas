unit GMOPCSrv;

interface

uses
  Windows, SysUtils, Classes, prOpcServer, prOpcTypes, GMGlobals;

const OPC_NO_REFRESH_INTERVAL = 300; // в секундах

type
  TOpcServer = class(TOpcItemServer)
  private
  protected
    function GetItemInfo(const ItemID: String; var AccessPath: string;
      var AccessRights: TAccessRights): Integer; override;
    procedure ReleaseHandle(ItemHandle: TItemHandle); override;
    procedure ListItemIDs(List: TItemIDList); override;
    procedure SetItemValue(ItemHandle: TItemHandle; const Value: OleVariant); override;
    function GetServerVersionIndependentID: string; override;
    function Options: TServerOptions; override;
    function GetItemVQT(ItemHandle: TItemHandle; var Quality: Word; var Timestamp: TFileTime): OleVariant; override;
  end;

  TChannelItem = class(TCollectionItem)
  public
    ID_Prm: int;
    Val: double;
    UTime: UINT;
    Tag: string;
    procedure AfterConstruction(); override;
  end;

  TChannelsCollection = class(TCollection)
  private
    FSynch: TMultiReadExclusiveWriteSynchronizer;

    function GetChannel(Index: int): TChannelItem;
    function GetChannelByID(ID_Prm: int): TChannelItem;
    function GetChannelByTag(Tag: string): TChannelItem;
  public
    function Add(): TChannelItem;
    property Channels[Index: int]: TChannelItem read GetChannel; default;
    property ByID[ID_Prm: int]: TChannelItem read GetChannelByID;
    property ByTag[Tag: string]: TChannelItem read GetChannelByTag;
    property synch: TMultiReadExclusiveWriteSynchronizer read FSynch;
    function FindOrAdd(ID_Prm: int): TChannelItem;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
  end;

var lstChannels: TChannelsCollection;

implementation

uses prOpcError, prOpcDa, DateUtils, Variants;

{ TOpcServer }

function TOpcServer.GetItemInfo(const ItemID: String; var AccessPath: string;
  var AccessRights: TAccessRights): Integer;
var c: TChannelItem;
begin
  {Return a handle that will subsequently identify ItemID}
  {raise exception of type EOpcError if Item ID not recognised}
  
  lstChannels.synch.BeginRead();
  try
    c := lstChannels.ByTag[ItemID];
    if c <> nil then
    begin
      Result := c.ID_Prm;
      AccessRights := [iaRead];
      AccessPath := 'Geomer';
    end
    else
      raise EOpcError.Create(OPC_E_INVALIDITEMID);
  finally
    lstChannels.synch.EndRead();
  end;
end;

procedure TOpcServer.ReleaseHandle(ItemHandle: TItemHandle);
begin
  {Release the handle previously returned by GetItemInfo}
end;

procedure TOpcServer.ListItemIds(List: TItemIDList);
var i: int;
begin
  {Call List.AddItemId(ItemId, AccessRights, VarType) for each ItemId}
  lstChannels.synch.BeginRead();
  try
    for i := 0 to lstChannels.Count - 1 do
      List.AddItemId(lstChannels[i].Tag, [iaRead], varDouble);
  finally
    lstChannels.synch.EndRead();
  end;
end;

function TOpcServer.GetItemVQT(ItemHandle: TItemHandle; var Quality: Word;
  var Timestamp: TFileTime): OleVariant;
var c: TChannelItem;
begin
  {return the value of the item identified by ItemHandle}
  lstChannels.synch.BeginRead();
  try
    c := lstChannels.ByID[ItemHandle];
    if c <> nil then
    begin
      Result := c.Val;
      Timestamp := DateTimeToFileTime(UTCtoLocal(c.UTime));

      if Abs(int64(c.UTime) - NowGM(TimeZone)) < OPC_NO_REFRESH_INTERVAL then
        Quality := OPC_QUALITY_GOOD
      else
      if c.UTime = 0 then
        Quality := OPC_QUALITY_COMM_FAILURE
      else
        Quality := OPC_QUALITY_LAST_KNOWN
    end
    else
    begin
      Result := Null;
      Quality := OPC_QUALITY_CONFIG_ERROR;
    end;
  finally
    lstChannels.synch.EndRead();
  end;
end;

procedure TOpcServer.SetItemValue(ItemHandle: TItemHandle; const Value: OleVariant);
begin
  {set the value of the item identified by ItemHandle}
end;

const
  ServerGuid: TGUID = '{705D98B7-368D-4D9A-9174-7DA56E92501F}';
  ServerVersion = 1;
  ServerDesc = 'Sirius ATM Opc Server';
  ServerVendor = 'Sirius ATM';

function TOpcServer.GetServerVersionIndependentID: string;
begin
  Result := 'SiriusATM';
end;

function TOpcServer.Options: TServerOptions;
begin
  Result := inherited Options + [soHierarchicalBrowsing];
end;

{ TChannelItem }

procedure TChannelItem.AfterConstruction;
begin
  inherited;

  ID_Prm := 0;
  UTime := 0;
  Val := 0;
  Tag := '';
end;                          

{ TChannelsCollection }

function TChannelsCollection.Add: TChannelItem;
begin
  Result := TChannelItem(inherited Add());
end;

procedure TChannelsCollection.AfterConstruction;
begin
  inherited;
  FSynch := TMultiReadExclusiveWriteSynchronizer.Create();
end;

procedure TChannelsCollection.BeforeDestruction;
begin
  inherited;
  FSynch.Free();
end;

function TChannelsCollection.FindOrAdd(ID_Prm: int): TChannelItem;
begin
  Result := ByID[ID_Prm];
  if Result = nil then
  begin
    Result := Add();
    Result.ID_Prm := ID_Prm;
  end;
end;

function TChannelsCollection.GetChannel(Index: int): TChannelItem;
begin
  Result := TChannelItem(Items[Index]);
end;

function TChannelsCollection.GetChannelByID(ID_Prm: int): TChannelItem;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Channels[i].ID_Prm = ID_Prm then
    begin
      Result := Channels[i];
      Exit;
    end;
  end;
end;

function TChannelsCollection.GetChannelByTag(Tag: string): TChannelItem;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Channels[i].Tag = Tag then
    begin
      Result := Channels[i];
      Exit;
    end;
  end;
end;

initialization
  lstChannels := TChannelsCollection.Create(TChannelItem);
  
  RegisterOPCServer(ServerGUID, ServerVersion, ServerDesc, ServerVendor, TOpcServer.Create);
finalization
  lstChannels.Free();
end.
