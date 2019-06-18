unit Frame.CurrentsDiagramBase;

interface

uses Windows, Forms, GMConst;

type
  TCurrentsDiagramBaseFrame = class(TFrame)
  protected
    function RealHandle(): HWND; virtual;
  public
    procedure RequestDiagrams();
    procedure CheckLastData();
    procedure RefreshObjectFrame();
  end;

implementation

{$R *.dfm}

function TCurrentsDiagramBaseFrame.RealHandle: HWND;
begin
  Result := Handle;
end;

procedure TCurrentsDiagramBaseFrame.RefreshObjectFrame;
begin
  if HandleAllocated then
    PostMessage(RealHandle(), WM_REFRESH_OBJECT_FRAME, 0, 0);
end;

procedure TCurrentsDiagramBaseFrame.RequestDiagrams;
begin
  if HandleAllocated then
    PostMessage(RealHandle(), WM_REQUEST_DIAGRAMS, 0, 0);
end;

procedure TCurrentsDiagramBaseFrame.CheckLastData;
begin
  if HandleAllocated then
    PostMessage(RealHandle(), WM_CHECK_LAST_DATA, 0, 0);
end;

end.
