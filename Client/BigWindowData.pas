unit BigWindowData;

interface

uses Windows, Classes, SysUtils, GMGlobals, ParamsForDiagram, ConfigXml, GMGenerics;

type
  TBWNodeData = record
    UT1, UT2: LongWord;
  end;
  PBWNodeData = ^TBWNodeData;

  TBigWindow = class
  private
    FPFD: TParamForDrawList;
    FDiagramLenHrs: int;
    function ReadAppropriateDigitsAfterPoint(xml: IGMConfigConfigType; ID_Prm: int): int;
  public
    Caption: string;

    constructor Create();
    destructor Destroy; override;
    property pfd: TParamForDrawList read FPFD;
    procedure LoadFromINI(xml: IGMConfigConfigType; xmlBw: IGMConfigBigwindowType);
    procedure DrawDiagram();
    function AddValue(val: TValueFromBase; bArchive: bool): bool;
    property DiagramLenHrs: int read FDiagramLenHrs write FDiagramLenHrs;
  end;

  TBigWindows = class(TGMCollection<TBigWindow>)
  public
    function AddValue(val: TValueFromBase; bArchive: bool): bool;
    procedure Enlist(lst: TStrings);
    procedure RequestInitialDiagrams(Handle: Hwnd);
    procedure LoadFromINI(xml: IGMConfigConfigType);
  end;

  var BigWindowList: TBigWindows;

implementation

uses GMDBClasses;

{ TBigWindows }

function TBigWindows.AddValue(val: TValueFromBase; bArchive: bool): bool;
var i: int;
begin
  Result := false;

  for i := 0 to Count - 1 do
    if Items[i].AddValue(val, bArchive) then
      Result := true;
end;

procedure TBigWindows.Enlist(lst: TStrings);
var i: int;
begin
  lst.Clear();
  for i := 0 to Count - 1 do
    lst.AddObject(Items[i].Caption, TObject(i));
end;

procedure TBigWindows.RequestInitialDiagrams(Handle: Hwnd);
var i: int;
begin
  for i := 0 to Count - 1 do
    Items[i].pfd.RequestInitialDiagrams(Handle, 1);
end;

procedure TBigWindows.LoadFromINI(xml: IGMConfigConfigType);
var i: int;
    bw: TBigWindow;
begin
  Clear();
  for i := 0 to xml.Bigwindows.Count - 1 do
  begin
    bw := BigWindowList.Add();
    bw.LoadFromINI(xml, xml.Bigwindows[i]);
  end;
end;

{ TBigWindow }

function TBigWindow.AddValue(val: TValueFromBase; bArchive: bool): bool;
begin
  Result := pfd.AddValue(val, bArchive);
end;

constructor TBigWindow.Create();
begin
  inherited;

  FPFD := TParamForDrawList.Create();
end;

destructor TBigWindow.Destroy;
begin
  FPFD.Free();
  inherited;
end;

procedure TBigWindow.DrawDiagram;
var i: int;
begin
  for i := 0 to pfd.Count - 1 do
    pfd.Params[i].DrawDiagram(int64(NowGM()) - FDiagramLenHrs * 3600, NowGM())
end;

function TBigWindow.ReadAppropriateDigitsAfterPoint(xml: IGMConfigConfigType; ID_Prm: int): int;
var
  i, j: int;
begin
  Result := 0;
  for i := 0 to xml.Objects.Count - 1 do
  begin
    for j := 1 to xml.Objects[i].Channels.Count - 1 do // 0й(BAR) не нужен
      if xml.Objects[i].Channels[j].Id_prm = ID_Prm then
      begin
        Result := xml.Objects[i].Channels[j].Digits;
        Exit;
      end;
  end;
end;

procedure TBigWindow.LoadFromINI(xml: IGMConfigConfigType; xmlBw: IGMConfigBigwindowType);
var
  i: int;
  p: TParamForDraw;
begin
  Caption := xmlBw.Name;
  for i := 0 to xmlBw.Diagrams.Count - 1 do
  begin
    p := pfd.Add();
    p.DiagramMin := xmlBw.Diagrams[i].Dmin;
    p.DiagramMax := xmlBw.Diagrams[i].Dmax;
    p.ColWidth := xmlBw.Diagrams[i].ColWidth;
    p.Title := xmlBw.Diagrams[i].Text;
    p.bDiagram := xmlBw.Diagrams[i].Diagram > 0;
    p.iDigitsAfterPoint := xmlBw.Diagrams[i].Precision + 1;

    p.prm := ParamOrNodeChannel(xmlBw.Diagrams[i].DataChnType, xmlBw.Diagrams[i].Id_prm);
    if (p.prm <> nil) and (p.iDigitsAfterPoint <= 0) then
      p.iDigitsAfterPoint := ReadAppropriateDigitsAfterPoint(xml, p.prm.ID_Prm);
  end;
end;

initialization
  BigWindowList := TBigWindows.Create();
finalization
  BigWindowList.Free();
end.
