unit Test.GBV;

interface

uses Windows, TestFrameWork, GMGlobals, GMBlockValues, Classes;

type
  TGeomerBlockValuesForTest = class(TGeomerBlockValues)
  protected
    function NeedTrace(): bool; override;
  end;

  TGBVTest = class(TTestCase)
  private
    gbv: TGeomerBlockValues;
    TracePath: string;
    BlockExamplePath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Trace();
    procedure StringForLog();
  end;

implementation

uses Forms, DateUtils, SysUtils, GMConst;

{ TGBVTest }

procedure TGBVTest.Trace();
var buf: array[0..100] of byte;
    i, fsize: int;
    fn: string;
    f: File;
begin
  for i := 0 to 100 do
    buf[i] := 0;

  fn := TracePath + '0_0_1.gbv';
  DeleteFile(fn);
  try
    gbv.ReadLongDataWithISCO(buf);
    Check(FileExists(fn), 'No File');

    AssignFile(f, fn);
    Reset(f, 1);
    fsize := FileSize(f);
    CloseFile(f);
    Check(fsize = 90, 'FileSize ' + IntToStr(fsize) + ' <> 90');
  finally
    DeleteFile(fn);
  end;
end;

procedure TGBVTest.SetUp;
begin
  inherited;
  gbv := TGeomerBlockValuesForTest.Create();
  TracePath := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'trace\';
  BlockExamplePath := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'examples\gbv\';
  RemoveDir(TracePath);
end;

procedure TGBVTest.StringForLog;
begin
  Check(gbv.ToString() <> 'TGeomerBlockValues');
end;

procedure TGBVTest.TearDown;
begin
  inherited;
  gbv.Free();
  RemoveDir(TracePath);
end;

{ TGeomerBlockValuesForTest }

function TGeomerBlockValuesForTest.NeedTrace: bool;
begin
  Result := true;
end;

initialization
  RegisterTest('GMIOPSrv/Classes/GBV', TGBVTest.Suite);
end.


