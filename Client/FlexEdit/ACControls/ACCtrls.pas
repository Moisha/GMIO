unit ACCtrls;

interface

uses
  Windows, Types, UITypes, Classes, Graphics, SysUtils, ExtCtrls,
  FlexBase, FlexProps, FlexUtils;

type
  TACControls = ( acNone, acZone, acDoor );

  TDoorState = ( dsClosed, dsOpened );
  TDoorOrientation = ( doVertical, doHorizontal );
  TDoorAccessDirection = ( ddUnknown, ddForward, ddBackward );
  TDoorEvent = ( deInactive, deValid, deError, deWarning, deBroken );

  TFlexDoor = class(TFlexControl)
  private
   FFrameIndex: integer;
   FLastFrameIndex: integer;
   FAnimTimer: TTimer;
   FPassAProp: TIntProp;
   FPassBProp: TIntProp;
   FOrientationProp: TEnumProp;
   FDoorStateProp: TEnumProp;
   FDoorEvent: TEnumProp;
   FAccessDir: TEnumProp;
   class procedure LoadResources;
   class procedure FreeResources;
   function  GetDoorOrientation: TDoorOrientation;
   function  GetDoorState: TDoorState;
   procedure SetDoorOrientation(Value: TDoorOrientation);
   procedure SetDoorState(Value: TDoorState);
   function  GetAccessDir: TDoorAccessDirection;
   function  GetDoorEvent: TDoorEvent;
   procedure SetAccessDir(Value: TDoorAccessDirection);
   procedure SetDoorEvent(Value: TDoorEvent);
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure StartAnim;
   procedure StopAnim;
   procedure AnimTimer(Sender: TObject); virtual;
   function  CreateArrowBitmap(Orient: TDoorOrientation; StyleIndex: integer;
     IsForward: boolean): TBitmap; virtual;
   function  CreateFrameBitmap(Orient: TDoorOrientation;
     Index: integer): TBitmap; virtual;
   function  CreateImage: TBitmap;
   property  OrientationProp: TEnumProp read FOrientationProp;
   property  DoorStateProp: TEnumProp read FDoorStateProp;
   property  DoorEventProp: TEnumProp read FDoorEvent;
  public
   class function GetToolInfo(ToolIcon: TBitmap; var Hint: string): boolean;
     override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   function  GetAnyExistPass: integer;
   property  PassAProp: TIntProp read FPassAProp;
   property  PassBProp: TIntProp read FPassBProp;
   property  Orientation: TDoorOrientation read GetDoorOrientation
     write SetDoorOrientation;
   property  DoorState: TDoorState read GetDoorState write SetDoorState;
   property  AccessDir: TDoorAccessDirection read GetAccessDir
     write SetAccessDir;
   property  DoorEvent: TDoorEvent read GetDoorEvent write SetDoorEvent;
  end;

procedure FindDoorsForPass(PassID: integer; List: TList);
function  GetACType(Control: TFlexControl): TACControls;

implementation

{$R Door.res Door.rc}

var
  FDoorImages: TBitmap;
  FDoorArrows: TBitmap;
  FDoorIcon: TBitmap;

  FDoorList: TList;

procedure FindDoorsForPass(PassID: integer; List: TList);
var i: integer;
begin
 List.Clear;
 for i:=0 to FDoorList.Count-1 do with TFlexDoor(FDoorList[i]) do
  if (PassID < 0) or
     (PassAProp.Value = PassID) or (PassBProp.Value = PassID) then
   List.Add(FDoorList[i]);
end;

function GetACType(Control: TFlexControl): TACControls;
var sType: string;
begin
 Result := acNone;
 if not Assigned(Control) then exit;
 sType := Control.UserData.Values['Type'];
 if sType = 'Door' then Result := acDoor else
 if sType = 'Zone' then Result := acZone;
end;

// TFlexDoor //////////////////////////////////////////////////////////////////

procedure TFlexDoor.ControlCreate;
begin
 UserData.Values['Type'] := 'Door';
 if Assigned(FDoorImages) then begin
  Width := ScalePixels(FDoorImages.Height div 2);
  Height := Width;
  FLastFrameIndex := FDoorImages.Width div UnScalePixels(Width) -1;
 end else begin
  Width := ScalePixels(20);
  Height := ScalePixels(20);
  FLastFrameIndex := 0;
 end;
 WidthProp.Style := WidthProp.Style + [psReadOnly];
 HeightProp.Style := HeightProp.Style + [psReadOnly];
 FFrameIndex := 0;
 if not Assigned(FDoorList) then FDoorList := TList.Create;
 FDoorList.Add(Self);
 inherited;
 Visible := True;
end;

procedure TFlexDoor.CreateProperties;
begin
 inherited;
 FPassAProp := TIntProp.Create(Props, 'PassA');
 FPassBProp := TIntProp.Create(Props, 'PassB');
 FOrientationProp := TEnumProp.Create(Props, 'Orientation');
 FOrientationProp.AddItem('Vertical');
 FOrientationProp.AddItem('Horizontal');
 FDoorStateProp := TEnumProp.Create(Props, 'DoorState');
 FDoorStateProp.AddItem('Closed');
 FDoorStateProp.AddItem('Opened');
 FAccessDir := TEnumProp.Create(Props, 'AccessDir');
 FAccessDir.AddItem('Unknown');
 FAccessDir.AddItem('Forward');
 FAccessDir.AddItem('Backward');
 FDoorEvent := TEnumProp.Create(Props, 'DoorEvent');
 FDoorEvent.AddItem('Inactive');
 FDoorEvent.AddItem('Valid');
 FDoorEvent.AddItem('Error');
 FDoorEvent.AddItem('Warning');
 FDoorEvent.AddItem('Broken');
end;

procedure TFlexDoor.ControlTranslate(const TranslateInfo: TTranslateInfo);
var Degree: integer;
begin
 inherited;
 Degree := TranslateInfo.Rotate mod 360 div 90;
 if Degree < 0 then Degree := 4 + Degree;
 if Degree and 1 <> 0 then
  FOrientationProp.EnumIndex := 1 - FOrientationProp.EnumIndex;  
end;

procedure TFlexDoor.ControlDestroy;
begin
 StopAnim;
 FDoorList.Remove(Self);
 if FDoorList.Count = 0 then FreeAndNil(FDoorList);
 inherited;
end;

class procedure TFlexDoor.LoadResources;
begin
 if not Assigned(FDoorImages) then FDoorImages := TBitmap.Create;
 FDoorImages.LoadFromResourceName(HInstance, 'DOORIMAGES');
 if not Assigned(FDoorArrows) then FDoorArrows := TBitmap.Create;
 FDoorArrows.LoadFromResourceName(HInstance, 'DOORARROWS');
 if not Assigned(FDoorIcon) then FDoorIcon := TBitmap.Create;
 FDoorIcon.LoadFromResourceName(HInstance, 'DOORICON');
end;

class procedure TFlexDoor.FreeResources;
begin
 FreeAndNil(FDoorImages);
 FreeAndNil(FDoorArrows);
 FreeAndNil(FDoorIcon);
end;

class function TFlexDoor.GetToolInfo(ToolIcon: TBitmap;
  var Hint: string): boolean;
begin
 Result := true;
 Hint := 'AC Door tool';
 if Assigned(ToolIcon) and Assigned(FDoorIcon) then
  ToolIcon.Canvas.Draw(0, 0, FDoorIcon);
end;

function TFlexDoor.GetDoorOrientation: TDoorOrientation;
begin
 Result := TDoorOrientation(FOrientationProp.EnumIndex);
end;

function TFlexDoor.GetDoorState: TDoorState;
begin
 Result := TDoorState(FDoorStateProp.EnumIndex);
end;

procedure TFlexDoor.SetDoorOrientation(Value: TDoorOrientation);
begin
 FOrientationProp.EnumIndex := integer(Value);
end;

procedure TFlexDoor.SetDoorState(Value: TDoorState);
begin
 FDoorStateProp.EnumIndex := integer(Value);
end;

function TFlexDoor.GetAccessDir: TDoorAccessDirection;
begin
 Result := TDoorAccessDirection(FAccessDir.EnumIndex);
end;

function TFlexDoor.GetDoorEvent: TDoorEvent;
begin
 Result := TDoorEvent(FDoorEvent.EnumIndex);
end;

procedure TFlexDoor.SetAccessDir(Value: TDoorAccessDirection);
begin
 FAccessDir.EnumIndex := integer(Value);
end;

procedure TFlexDoor.SetDoorEvent(Value: TDoorEvent);
begin
 FDoorEvent.EnumIndex := integer(Value);
end;

function TFlexDoor.GetAnyExistPass: integer;
begin
 if PassAProp.Value > 0 then
  Result := PassAProp.Value
 else
 if PassBProp.Value > 0 then
  Result := PassBProp.Value
 else
  Result := 0;
end;

function TFlexDoor.CreateArrowBitmap(Orient: TDoorOrientation;
  StyleIndex: integer; IsForward: boolean): TBitmap;
var Img: TPoint;
    Size: integer;
begin
 if not Assigned(FDoorArrows) then begin
  Result := Nil;
  exit;
 end;
 Img.Y := StyleIndex;
 if Orient = doHorizontal then begin
  if IsForward
   then Img.X := 2
   else Img.X := 1;
 end else
  if IsForward
   then Img.X := 3
   else Img.X := 0;
 Size := (FDoorArrows.Width div 4);
 Img.X := Img.X * Size;
 Img.Y := Img.Y * Size;
 Result := TBitmap.Create;
 Result.Width := Size;
 Result.Height := Size;
 Result.Canvas.CopyRect(Rect(0, 0, Size, Size), FDoorArrows.Canvas,
   Rect(Img.X, Img.Y, Img.X + Size, Img.Y + Size));
 Result.Transparent := True;
end;

function TFlexDoor.CreateFrameBitmap(Orient: TDoorOrientation;
  Index: integer): TBitmap;
var Img: TPoint;
    PicSize: TPoint;
begin
 if not Assigned(FDoorImages) then begin
  Result := Nil;
  exit;
 end;
 PicSize.X := UnScalePixels(Width);
 PicSize.Y := UnScalePixels(Height);
 Img.X := Index * PicSize.X;
 if Orient = doVertical
  then Img.Y := 0
  else Img.Y := PicSize.Y;
 Result := TBitmap.Create;
 Result.Width := PicSize.X;
 Result.Height := PicSize.Y;
 Result.Canvas.CopyRect(Rect(0, 0, PicSize.X, PicSize.Y), FDoorImages.Canvas,
   Rect(Img.X, Img.Y, Img.X + PicSize.X, Img.Y + PicSize.Y));
 Result.Transparent := True;
end;

function TFlexDoor.CreateImage: TBitmap;
var Arrow: TBitmap;
    Img: TPoint;
begin
 Result := CreateFrameBitmap(Orientation, FFrameIndex);
 if not Assigned(Result) then exit;
 if ((DoorState = dsOpened) or Assigned(FAnimTimer)) and
    (AccessDir <> ddUnknown) or (DoorEvent = deBroken) then begin
  Arrow := CreateArrowBitmap(Orientation, integer(DoorEvent),
    AccessDir = ddForward);
  Img.X := (UnScalePixels(Width) - Arrow.Width) div 2;
  Img.Y := (UnScalePixels(Height) - Arrow.Height) div 2;
  if Assigned(Arrow) then Result.Canvas.Draw(Img.X, Img.Y, Arrow);
  Arrow.Free;
 end;
end;

procedure TFlexDoor.Paint(Canvas: TCanvas; var PaintRect: TRect);
var Frame: TBitmap;
begin
 if not Assigned(FDoorImages) then exit;
 Frame := CreateImage;
 try
  Canvas.StretchDraw(PaintRect, Frame);
 finally
  Frame.Free;
 end;
end;

function TFlexDoor.IsPointInside(PaintX, PaintY: integer): boolean;
var Frame: TBitmap;
    P: TPoint;
begin
 Result := inherited IsPointInside(PaintX, PaintY);
 if not Result then exit;
 P := OwnerToClient(Point(PaintX, PaintY));
 Frame := CreateFrameBitmap(Orientation, 0);
 try
  if Frame.Canvas.Pixels[P.X, P.Y] and $FFFFFF =
   Frame.TransparentColor and $FFFFFF then Result := False;
 finally
  Frame.Free;
 end;
end;

procedure TFlexDoor.StartAnim;
begin
 if not Assigned(FAnimTimer) then begin
  FAnimTimer := TTimer.Create(Nil);
  FAnimTimer.Interval := 20;
  FAnimTimer.OnTimer := AnimTimer;
 end;
 AnimTimer(FAnimTimer);
end;

procedure TFlexDoor.StopAnim;
begin
 FreeAndNil(FAnimTimer);
 Invalidate;
end;

procedure TFlexDoor.AnimTimer(Sender: TObject);
begin
 FAnimTimer.Enabled := False;
 try
  if DoorState = dsClosed then begin
   if FFrameIndex > 0 then begin
    dec(FFrameIndex);
    Invalidate;
   end else
    StopAnim;
  end else
  if DoorState = dsOpened then begin
   if FFrameIndex < FLastFrameIndex then begin
    inc(FFrameIndex);
    Invalidate;
   end else
    StopAnim;
  end else
   StopAnim;
 finally
  if Assigned(FAnimTimer) then FAnimTimer.Enabled := True;
 end;
end;

procedure TFlexDoor.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 inherited;
 if Prop = FDoorStateProp then StartAnim;
end;

///////////////////////////////////////////////////////////////////////////////

procedure RegisterACControls;
begin
 RegisterFlexControl(TFlexDoor);
end;

initialization
  TFlexDoor.LoadResources;
  RegisterACControls;

finalization
  TFlexDoor.FreeResources;

end.
