unit Test.RequestListBuilder;

interface

uses TestFrameWork, GMGlobals, Threads.ReqSpecDevTemplate, RequestList;

type
  TRequestListBuilderTest = class(TTestCase)
  private
    FRequestCollection: TRequestCollection;
    FBuilder: TRequestListBuilder;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
  published
    procedure TestBuild;
  end;

implementation

uses GMConst;

{ TRequestListBuilderTest }

procedure TRequestListBuilderTest.SetUp;
begin
  inherited;
  FRequestCollection := TRequestCollection.Create();
  FBuilder := TRequestListBuilder.Create(OBJ_TYPE_COM, 6);
end;

procedure TRequestListBuilderTest.TearDown;
begin
  inherited;
  FBuilder.Free();
  FRequestCollection.Free();
end;

procedure TRequestListBuilderTest.TestBuild;
begin
  FBuilder.Build(FRequestCollection);
  Check(FRequestCollection.Count > 0);
end;

initialization
  RegisterTest('GMIOPSrv/RequestListBuilder', TRequestListBuilderTest.Suite);

end.
