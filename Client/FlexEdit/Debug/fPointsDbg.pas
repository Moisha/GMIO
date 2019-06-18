unit fPointsDbg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, FlexBase, FlexUtils, FlexPath;

type
  TfmPointsDebug = class(TForm)
    lvPoints: TListView;
    sbrMain: TStatusBar;
    tmRefresh: TTimer;
    procedure tmRefreshTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FActiveFlex: TFlexPanel;
    FActiveControl: TFlexControl;
    procedure UpdateData(DoAdd: boolean);
  public
    { Public declarations }
  end;

var
 fmPointsDebug: TfmPointsDebug;

procedure ShowPointsDebug;

implementation

uses Main;

{$R *.DFM}

procedure ShowPointsDebug;
begin
 if not Assigned(fmPointsDebug) then
  fmPointsDebug := TfmPointsDebug.Create(Application);
 fmPointsDebug.Show;
end;

procedure TfmPointsDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 fmPointsDebug := Nil;
end;

procedure TfmPointsDebug.tmRefreshTimer(Sender: TObject);
var Control: TFlexControl;
begin
 if Assigned(EditMainForm) then begin
  Control := Nil;
  // Check ActiveFlex
  if EditMainForm.ActiveFlex <> FActiveFlex then
   FActiveFlex := EditMainForm.ActiveFlex;
  // Check ActiveControl
  if Assigned(FActiveFlex) and (FActiveFlex.SelectedCount = 1) and
    (FActiveFlex.Selected[0].PointCount > 0) then
   Control := FActiveFlex.Selected[0];
  // Check update
  if Control <> FActiveControl then begin
   FActiveControl := Control;
   UpdateData(True);
  end else
   UpdateData(False);
 end;
end;

procedure TfmPointsDebug.UpdateData(DoAdd: boolean);
var i, j: integer;
    ScreenPoints: TPointArray;
    Item: TListItem;
begin
 ScreenPoints := Nil;
 lvPoints.Items.BeginUpdate;
 try
  if not Assigned(FActiveControl) or
    (lvPoints.Items.Count <> FActiveControl.PointCount) then DoAdd := true;
  if DoAdd then lvPoints.Items.Clear;
  if not Assigned(FActiveControl) then exit;
  with FActiveFlex, FActiveControl.PaintRect do
   ScreenPoints := FActiveControl.GetTransformPoints(Left, Top, Scale);
  for i:=0 to FActiveControl.PointCount-1 do begin
   if DoAdd
    then Item := lvPoints.Items.Add
    else Item := lvPoints.Items[i];
   Item.Caption := IntToStr(i);
   if DoAdd then for j:=1 to 5 do Item.SubItems.Add('');
   case FActiveControl.PointTypes[i] of
    ptNode         : Item.SubItems[0] := 'Node';
    ptEndNode      : Item.SubItems[0] := 'EndNode';
    ptEndNodeClose : Item.SubItems[0] := 'EndNodeClose';
    ptControl      : Item.SubItems[0] := 'Control';
   end;
   Item.SubItems[1] := IntToStr(FActiveControl.Points[i].x);
   Item.SubItems[2] := IntToStr(FActiveControl.Points[i].y);
   Item.SubItems[3] := IntToStr(ScreenPoints[i].x);
   Item.SubItems[4] := IntToStr(ScreenPoints[i].y);
  end;
 finally
  lvPoints.Items.EndUpdate;
 end;
 with FActiveControl do begin
  sbrMain.Panels[0].Text := Format('Pos [%d, %d]', [Left, Top]);
  sbrMain.Panels[1].Text := Format('Size [%d, %d]', [Width, Height]);
 end;
end;

end.
