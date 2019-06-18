unit StdRequest;

interface

uses xmldom, XMLDoc, XMLIntf, Variants;

type

{ Forward Decls }

  IXMLGMIORequestType = interface;
  IXMLAuthType = interface;
  IXMLDataType = interface;
  IXMLSourcesType = interface;
  IXMLSourceType = interface;
  IXMLStateType = interface;

{ IXMLGMIORequestType }

  IXMLGMIORequestType = interface(IXMLNode)
    ['{086C2F9B-BA0D-48EA-9853-0595CC0716E3}']
    { Property Accessors }
    function Get_Auth: IXMLAuthType;
    function Get_Structure: Integer;
    function Get_Data: IXMLDataType;
    function Get_State: IXMLStateType;
    function Get_Comment: UnicodeString;
    procedure Set_Comment(Value: UnicodeString);
    procedure Set_Structure(Value: Integer);
    { Methods & Properties }
    property Comment: UnicodeString read Get_Comment write Set_Comment;
    property Auth: IXMLAuthType read Get_Auth;
    property Structure: Integer read Get_Structure write Set_Structure;
    property Data: IXMLDataType read Get_Data;
    property State: IXMLStateType read Get_State;
  end;

{ IXMLStateType }

  IXMLStateType = interface(IXMLNode)
    ['{9DD441AD-60C6-41B0-B32F-125537230BB3}']
    { Property Accessors }
    function Get_RemoteServerReady: integer;
    procedure Set_RemoteServerReady(Value: integer);
    { Methods & Properties }
    property RemoteServerReady: integer read Get_RemoteServerReady write Set_RemoteServerReady;
  end;

{ IXMLAuthType }

  IXMLAuthType = interface(IXMLNode)
    ['{F40CB462-EE38-47FE-800D-65302BD0A52B}']
    { Property Accessors }
    function Get_Login: UnicodeString;
    function Get_Password: UnicodeString;
    function Get_RemoteName: UnicodeString;
    function Get_RequestID: UnicodeString;
    procedure Set_RequestID(Value: UnicodeString);
    procedure Set_RemoteName(Value: UnicodeString);
    procedure Set_Login(Value: UnicodeString);
    procedure Set_Password(Value: UnicodeString);
    { Methods & Properties }
    property Login: UnicodeString read Get_Login write Set_Login;
    property Password: UnicodeString read Get_Password write Set_Password;
    property RemoteName: UnicodeString read Get_RemoteName write Set_RemoteName;
    property RequestID: UnicodeString read Get_RequestID write Set_RequestID;
  end;

{ IXMLDataType }

  IXMLDataType = interface(IXMLNode)
    ['{248C3598-ED18-4D08-B4C3-63FB66922C72}']
    { Property Accessors }
    function Get_Sources: IXMLSourcesType;
    function Get_SQL: UnicodeString;
    function Get_OPC: Integer;
    procedure Set_SQL(Value: UnicodeString);
    procedure Set_OPC(Value: Integer);
    { Methods & Properties }
    property Sources: IXMLSourcesType read Get_Sources;
    property SQL: UnicodeString read Get_SQL write Set_SQL;
    property OPC: Integer read Get_OPC write Set_OPC;
  end;

{ IXMLSourcesType }

  IXMLSourcesType = interface(IXMLNodeCollection)
    ['{621FD3F6-CCB5-4717-8DB0-6EEC42CDA866}']
    { Property Accessors }
    function Get_Source(Index: Integer): IXMLSourceType;
    { Methods & Properties }
    function Add: IXMLSourceType;
    function Insert(const Index: Integer): IXMLSourceType;
    property Source[Index: Integer]: IXMLSourceType read Get_Source; default;
  end;

{ IXMLSourceType }

  IXMLSourceType = interface(IXMLNode)
    ['{AEAAFC24-F86F-4D32-A214-545D9B04C4AE}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_ValSrcType: Integer;
    function Get_ValType: Integer;
    function Get_AggrType: Integer;
    function Get_UTime1: Cardinal;
    function Get_UTime2: Cardinal;
    function Get_AggrIntervalLen: Integer;
    function Get_DiagramType: Integer;
    procedure Set_DiagramType(Value: Integer);
    procedure Set_AggrIntervalLen(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_ValSrcType(Value: Integer);
    procedure Set_ValType(Value: Integer);
    procedure Set_AggrType(Value: Integer);
    procedure Set_UTime1(Value: Cardinal);
    procedure Set_UTime2(Value: Cardinal);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property ValSrcType: Integer read Get_ValSrcType write Set_ValSrcType;
    property ValType: Integer read Get_ValType write Set_ValType;
    property DiagramType: Integer read Get_DiagramType write Set_DiagramType;
    property AggrType: Integer read Get_AggrType write Set_AggrType;
    property AggrIntervalLen: Integer read Get_AggrIntervalLen write Set_AggrIntervalLen;
    property UTime1: Cardinal read Get_UTime1 write Set_UTime1;
    property UTime2: Cardinal read Get_UTime2 write Set_UTime2;
  end;

{ Forward Decls }

  TXMLGMIORequestType = class;
  TXMLAuthType = class;
  TXMLDataType = class;
  TXMLSourcesType = class;
  TXMLSourceType = class;

{ TXMLGMIORequestType }

  TXMLGMIORequestType = class(TXMLNode, IXMLGMIORequestType)
  protected
    { IXMLGMIORequestType }
    function Get_Auth: IXMLAuthType;
    function Get_Structure: Integer;
    function Get_Data: IXMLDataType;
    function Get_State: IXMLStateType;
    function Get_Comment: UnicodeString;
    procedure Set_Comment(Value: UnicodeString);
    procedure Set_Structure(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ IXMLStateType }

  TXMLStateType = class(TXMLNode, IXMLStateType)
  protected
    { IXMLStateType }
    function Get_RemoteServerReady: integer;
    procedure Set_RemoteServerReady(Value: integer);
  end;

{ TXMLAuthType }

  TXMLAuthType = class(TXMLNode, IXMLAuthType)
  protected
    { IXMLAuthType }
    function Get_Login: UnicodeString;
    function Get_Password: UnicodeString;
    function Get_RemoteName: UnicodeString;
    function Get_RequestID: UnicodeString;
    procedure Set_RequestID(Value: UnicodeString);
    procedure Set_RemoteName(Value: UnicodeString);
    procedure Set_Login(Value: UnicodeString);
    procedure Set_Password(Value: UnicodeString);
  end;

{ TXMLDataType }

  TXMLDataType = class(TXMLNode, IXMLDataType)
  protected
    { IXMLDataType }
    function Get_Sources: IXMLSourcesType;
    function Get_SQL: UnicodeString;
    function Get_OPC: Integer;
    procedure Set_SQL(Value: UnicodeString);
    procedure Set_OPC(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSourcesType }

  TXMLSourcesType = class(TXMLNodeCollection, IXMLSourcesType)
  protected
    { IXMLSourcesType }
    function Get_Source(Index: Integer): IXMLSourceType;
    function Add: IXMLSourceType;
    function Insert(const Index: Integer): IXMLSourceType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSourceType }

  TXMLSourceType = class(TXMLNode, IXMLSourceType)
  protected
    { IXMLSourceType }
    function Get_Id: Integer;
    function Get_ValSrcType: Integer;
    function Get_ValType: Integer;
    function Get_AggrType: Integer;
    function Get_UTime1: Cardinal;
    function Get_UTime2: Cardinal;
    function Get_AggrIntervalLen: Integer;
    function Get_DiagramType: Integer;
    procedure Set_DiagramType(Value: Integer);
    procedure Set_AggrIntervalLen(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_ValSrcType(Value: Integer);
    procedure Set_ValType(Value: Integer);
    procedure Set_AggrType(Value: Integer);
    procedure Set_UTime1(Value: Cardinal);
    procedure Set_UTime2(Value: Cardinal);
  end;

{ Global Functions }

function GetGMIORequest(Doc: IXMLDocument): IXMLGMIORequestType;
function LoadGMIORequest(const FileName: string): IXMLGMIORequestType;
function NewGMIORequest: IXMLGMIORequestType;
function LoadXMLData_GMIORequest(const xml: string): IXMLGMIORequestType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

uses GMGlobals;

function GetGMIORequest(Doc: IXMLDocument): IXMLGMIORequestType;
begin
  Result := Doc.GetDocBinding('GMIORequest', TXMLGMIORequestType, TargetNamespace) as IXMLGMIORequestType;
end;

function LoadGMIORequest(const FileName: string): IXMLGMIORequestType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('GMIORequest', TXMLGMIORequestType, TargetNamespace) as IXMLGMIORequestType;
end;

function NewGMIORequest: IXMLGMIORequestType;
begin
  Result := NewXMLDocument.GetDocBinding('GMIORequest', TXMLGMIORequestType, TargetNamespace) as IXMLGMIORequestType;
  Result.Auth.RequestID := GenerateGUID();
end;

function LoadXMLData_GMIORequest(const xml: string): IXMLGMIORequestType;
begin
  Result := LoadXMLData(xml).GetDocBinding('GMIORequest', TXMLGMIORequestType, TargetNamespace) as IXMLGMIORequestType;
end;

{ TXMLGMIORequestType }

procedure TXMLGMIORequestType.AfterConstruction;
begin
  RegisterChildNode('Auth', TXMLAuthType);
  RegisterChildNode('Data', TXMLDataType);
  RegisterChildNode('State', TXMLStateType);
  inherited;
end;

function TXMLGMIORequestType.Get_Auth: IXMLAuthType;
begin
  Result := ChildNodes['Auth'] as IXMLAuthType;
end;

function TXMLGMIORequestType.Get_Comment: UnicodeString;
begin
  Result := AttributeNodes['Comment'].Text;
end;

function TXMLGMIORequestType.Get_State: IXMLStateType;
begin
 Result := ChildNodes['State'] as IXMLStateType;
end;

function TXMLGMIORequestType.Get_Structure: Integer;
begin
  Result := ChildNodes['Structure'].NodeValue;
end;

procedure TXMLGMIORequestType.Set_Comment(Value: UnicodeString);
begin
  AttributeNodes['Comment'].Text := Value;
end;

procedure TXMLGMIORequestType.Set_Structure(Value: Integer);
begin
  ChildNodes['Structure'].NodeValue := Value;
end;

function TXMLGMIORequestType.Get_Data: IXMLDataType;
begin
  Result := ChildNodes['Data'] as IXMLDataType;
end;

{ TXMLAuthType }

function TXMLAuthType.Get_Login: UnicodeString;
begin
  Result := AttributeNodes['Login'].Text;
end;

procedure TXMLAuthType.Set_Login(Value: UnicodeString);
begin
  SetAttribute('Login', Value);
end;

function TXMLAuthType.Get_Password: UnicodeString;
begin
  Result := AttributeNodes['Password'].Text;
end;

function TXMLAuthType.Get_RemoteName: UnicodeString;
begin
  Result := AttributeNodes['RemoteName'].Text;
end;

function TXMLAuthType.Get_RequestID: UnicodeString;
begin
  Result := AttributeNodes['RequestID'].Text;
end;

procedure TXMLAuthType.Set_Password(Value: UnicodeString);
begin
  SetAttribute('Password', Value);
end;

procedure TXMLAuthType.Set_RemoteName(Value: UnicodeString);
begin
  SetAttribute('RemoteName', Value);
end;

procedure TXMLAuthType.Set_RequestID(Value: UnicodeString);
begin
  SetAttribute('RequestID', Value);
end;

{ TXMLDataType }

procedure TXMLDataType.AfterConstruction;
begin
  RegisterChildNode('Sources', TXMLSourcesType);
  inherited;
end;

function TXMLDataType.Get_Sources: IXMLSourcesType;
begin
  Result := ChildNodes['Sources'] as IXMLSourcesType;
end;

function TXMLDataType.Get_SQL: UnicodeString;
begin
  Result := ChildNodes['SQL'].Text;
end;

procedure TXMLDataType.Set_SQL(Value: UnicodeString);
begin
  ChildNodes['SQL'].NodeValue := Value;
end;

function TXMLDataType.Get_OPC: Integer;
begin
  Result := ChildNodes['OPC'].NodeValue;
end;

procedure TXMLDataType.Set_OPC(Value: Integer);
begin
  ChildNodes['OPC'].NodeValue := Value;
end;

{ TXMLSourcesType }

procedure TXMLSourcesType.AfterConstruction;
begin
  RegisterChildNode('Source', TXMLSourceType);
  ItemTag := 'Source';
  ItemInterface := IXMLSourceType;
  inherited;
end;

function TXMLSourcesType.Get_Source(Index: Integer): IXMLSourceType;
begin
  Result := List[Index] as IXMLSourceType;
end;

function TXMLSourcesType.Add: IXMLSourceType;
begin
  Result := AddItem(-1) as IXMLSourceType;
end;

function TXMLSourcesType.Insert(const Index: Integer): IXMLSourceType;
begin
  Result := AddItem(Index) as IXMLSourceType;
end;

{ TXMLSourceType }

function TXMLSourceType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLSourceType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLSourceType.Get_ValSrcType: Integer;
begin
  Result := AttributeNodes['ValSrcType'].NodeValue;
end;

procedure TXMLSourceType.Set_ValSrcType(Value: Integer);
begin
  SetAttribute('ValSrcType', Value);
end;

function TXMLSourceType.Get_ValType: Integer;
begin
  Result := AttributeNodes['ValType'].NodeValue;
end;

procedure TXMLSourceType.Set_ValType(Value: Integer);
begin
  SetAttribute('ValType', Value);
end;

function TXMLSourceType.Get_AggrIntervalLen: Integer;
begin
  Result := AttributeNodes['AggrIntrvlLen'].NodeValue;
end;

function TXMLSourceType.Get_AggrType: Integer;
begin
  Result := AttributeNodes['AggrType'].NodeValue;
end;

function TXMLSourceType.Get_DiagramType: Integer;
begin
  Result := AttributeNodes['DiagramType'].NodeValue;
end;

procedure TXMLSourceType.Set_AggrIntervalLen(Value: Integer);
begin
  SetAttribute('AggrIntrvlLen', Value);
end;

procedure TXMLSourceType.Set_AggrType(Value: Integer);
begin
  SetAttribute('AggrType', Value);
end;

procedure TXMLSourceType.Set_DiagramType(Value: Integer);
begin
  SetAttribute('DiagramType', Value);
end;

function TXMLSourceType.Get_UTime1: Cardinal;
begin
  Result := AttributeNodes['UTime1'].NodeValue;
end;

procedure TXMLSourceType.Set_UTime1(Value: Cardinal);
begin
  SetAttribute('UTime1', Value);
end;

function TXMLSourceType.Get_UTime2: Cardinal;
begin
  Result := AttributeNodes['UTime2'].NodeValue;
end;

procedure TXMLSourceType.Set_UTime2(Value: Cardinal);
begin
  SetAttribute('UTime2', Value);
end;

{ TXMLStateType }

function TXMLStateType.Get_RemoteServerReady: integer;
begin
  Result := ChildNodes['RemoteServerReady'].NodeValue;
end;

procedure TXMLStateType.Set_RemoteServerReady(Value: integer);
begin
  ChildNodes['RemoteServerReady'].NodeValue := Value;
end;

initialization
  NullStrictConvert := false;
end.
