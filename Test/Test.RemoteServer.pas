unit Test.RemoteServer;

interface

uses Windows, TestFrameWork, GMGlobals, GMConst, StdResponce, ConnParamsStorage, ClientResponce;

type
  TRemoteServerTest = class(TTestCase)
  private
    processor: TClientResponce;
  protected
    resp: IXMLGMIOResponceType;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Structure();
  end;

implementation

{ TRemoteServerTest }

procedure TRemoteServerTest.SetUp;
begin
  inherited;

  processor := TClientResponce.Create(nil);
end;

procedure TRemoteServerTest.Structure;
begin
  resp := processor.DBStructure();
  Check(resp <> nil, 'nil');
  // resp.OwnerDocument.SaveToFile('c:\remote.xml');
end;

procedure TRemoteServerTest.TearDown;
begin
  resp := nil;
  processor.Free();

  inherited;
end;

initialization
  RegisterTest('GMIOPSrv/RemoteServer', TRemoteServerTest.Suite);
end.
