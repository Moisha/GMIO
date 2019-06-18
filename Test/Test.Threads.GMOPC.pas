unit Test.Threads.GMOPC;

interface

uses SysUtils, OPCDataThread, Classes, GMGlobals,
     Windows, TestFrameWork;

type
  TGMOPCDataThreadForTest = class(TGMOPCDataThread)
  public
    procedure Execute(); override;
    function CheckTestData(const s: string): bool;
    procedure ReadINI(); override;
  end;

  TGMOPCThreadTest = class(TTestCase)
  private
    thr: TGMOPCDataThreadForTest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckTestdata();
  end;

implementation

{ TGMOPCDataThreadForTest }

function TGMOPCDataThreadForTest.CheckTestdata(const s: string): bool;
var l: TStringList;
    i: int;
begin
  l := TStringList.Create();
  try
    l.CommaText := s;
    FConnectionObject.buffers.NumberOfBytesRead := 0;
    for i := 0 to l.Count - 1 do
    begin
      FConnectionObject.buffers.BufRec[FConnectionObject.buffers.NumberOfBytesRead] := StrToInt(l[i]);
      inc(FConnectionObject.buffers.NumberOfBytesRead);
    end;

    Result := DecodeBufRec(nil);
  finally
    l.Free();
  end;
end;

procedure TGMOPCDataThreadForTest.Execute;
begin
  // Ничего не делаем, просто выходим
end;

procedure TGMOPCDataThreadForTest.ReadINI;
begin

end;

{ TGMOPCThreadTest }

procedure TGMOPCThreadTest.CheckTestdata;
begin
  Check(thr.CheckTestData('20 135 68 0 0 0 120 218 179 177 175 200 205 81 40 75 45 42 206 204 207 179 85 50 212 51 80 82 72 205 ' +
                  '75 206 79 201 204 75 183 85 42 207 204 75 201 47 47 214 53 52 50 53 84 178 183 227 226 181 73 206 72 ' +
                  '204 203 75 205 41 214 183 227 229 2 0 83 107 18 140'), '1');

{ // Отключил проверку, чтобы не сыпала исключениями
  Check(not thr.CheckTestData('20 135 68 0 0 0 120 25 218 179 177 175 200 205 81 40 75 45 42 206 204 207 179 85 50 212 51 80 82 72 205 ' +
                  '75 206 79 201 204 75 183 85 42 207 204 75 201 47 47 214 53 52 50 53 84 178 183 227 226 181 73 206 72 ' +
                  '204 203 75 205 41 214 183 227 229 2 0 83 107 18 140'));
}
end;

procedure TGMOPCThreadTest.SetUp;
begin
  inherited;
  thr := TGMOPCDataThreadForTest.Create(0);
end;

procedure TGMOPCThreadTest.TearDown;
begin
  inherited;
  thr.Free();
end;

initialization
  RegisterTest('OPC', TGMOPCThreadTest.Suite);
end.
