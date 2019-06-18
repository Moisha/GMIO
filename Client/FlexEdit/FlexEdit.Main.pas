unit FlexEdit.Main;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Menus, ImgList, ActnList, ExtCtrls, StdActns, Buttons,
  ClipBrd, Printers, Jpeg, fChild, fOptions, Grids, dxColorDialog, dxCoreGraphics,
  TB2ToolWindow, TB2Item, TB2Dock, TB2Toolbar, RxGIF, 
  {$IFDEF USE_FLEXPLUS} FlexPlus, {$ENDIF}
  FlexBase, FlexProps, FlexUtils, FlexControls, FlexPath, FlexFileFormats, System.Actions,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, cxClasses;

{.DEFINE DEBUG_HISTORY}

{.DEFINE DEBUG_POINTS}

type
  TEditMainForm = class(TForm)
    sbrMain: TStatusBar;
    imgToolIcons: TImageList;
    imgStdIcons: TImageList;
    pmControl: TPopupMenu;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miEdit: TMenuItem;
    miArrange: TMenuItem;
    miWindow: TMenuItem;
    miToolBarsDelimiter: TMenuItem;
    miAddToLibrary: TMenuItem;
    miView: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    BackOne1: TMenuItem;
    ForwardOne1: TMenuItem;
    Mnemo1: TMenuItem;
    Newscheme1: TMenuItem;
    Deletescheme1: TMenuItem;
    Newlayer1: TMenuItem;
    Deletelayer1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    Cascade1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    MinimizeAll1: TMenuItem;
    BackOne2: TMenuItem;
    Close1: TMenuItem;
    Palette1: TMenuItem;
    Clone1: TMenuItem;
    SaveAs1: TMenuItem;
    Properties1: TMenuItem;
    BackOne3: TMenuItem;
    ForwardOne2: TMenuItem;
    Toback1: TMenuItem;
    Tofront1: TMenuItem;
    Group1: TMenuItem;
    Ungroup1: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    Delete2: TMenuItem;
    Options1: TMenuItem;
    Print1: TMenuItem;
    Duplicate1: TMenuItem;
    Duplicate2: TMenuItem;
    Inspector1: TMenuItem;
    Library1: TMenuItem;
    Userdata1: TMenuItem;
    Prview1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    Layermanager1: TMenuItem;

    alMain: TActionList;
    acFileNew: TAction;
    acFileExit: TAction;
    acFileOpen: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditPaste: TAction;
    acEditClone: TAction;
    acEditUndo: TAction;
    acEditRedo: TAction;
    acFilePrint: TAction;
    acFileImport: TAction;
    acFileExport: TAction;
    acArrangeForwardOne: TAction;
    acArrangeBackOne: TAction;
    acArrangeToFront: TAction;
    acArrangeToBack: TAction;
    acArrangeGroup: TAction;
    acArrangeUngroup: TAction;
    acLayerNew: TAction;
    acLayerDelete: TAction;
    acEditDelete: TAction;
    acSchemeNew: TAction;
    acSchemeDelete: TAction;
    acArrangeToBack1: TMenuItem;
    acArrangeToFront1: TMenuItem;
    acArrangeGroup1: TMenuItem;
    acArrangeUngroup1: TMenuItem;
    acWindowArrange: TWindowArrange;
    acWindowCascade: TWindowCascade;
    acWindowClose: TWindowClose;
    acWindowMinimizeAll: TWindowMinimizeAll;
    acWindowTileHorizontal: TWindowTileHorizontal;
    acWindowTileVertical: TWindowTileVertical;
    acFileProperties: TAction;
    acDockerInspector: TAction;
    acDockerLibrary: TAction;
    acDockerLayers: TAction;
    acDockerPalette: TAction;
    acAlignLeft: TAction;
    acAlignHCenter: TAction;
    acAlignRight: TAction;
    acAlignTop: TAction;
    acAlignVCenter: TAction;
    acAlignBottom: TAction;
    acAlignCenter: TAction;
    acLibItemAdd: TAction;
    acViewOptions: TAction;
    acFilePreview: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    acZoomActual: TAction;
    acEditDuplicate: TAction;
    acTranslateRotateCW: TAction;
    acTranslateRotateCCW: TAction;
    acTranslateFlipHorz: TAction;
    acTranslateFlipVertical: TAction;
    acDockerUserData: TAction;
    acGridShow: TAction;
    acGridPixelShow: TAction;
    acGridSnap: TAction;
    acGridOptions: TAction;
    acLayerToBack: TAction;
    acLayerToFront: TAction;
    acDebugPoints: TAction;
    acCurveJoin: TAction;
    acCurveBreak: TAction;
    acCurveClose: TAction;
    acCurveToLine: TAction;
    acCurveToCurve: TAction;
    acCurveBreakApart: TAction;
    acCurveCombine: TAction;
    acCurveFlatten: TAction;
    acCurveConvertToCurve: TAction;
    acDebugHistory: TAction;

    pd_Main: TPrintDialog;
    od_Main: TOpenDialog;
    sd_Main: TSaveDialog;
    cd_Palette: TdxColorDialog;
    od_Import: TOpenDialog;
    sd_Export: TSaveDialog;

    panColors: TcxGroupBox;
    dgColors: TDrawGrid;
    sptColors: TcxSplitter;

    cbZoom: TcxComboBox;
    cbActiveLayer: TcxComboBox;
    cbActiveScheme: TcxComboBox;
    
    tbrTools: TTBToolbar;
    tbrStdTools: TTBToolbar;
    tbrZoomTools: TTBToolbar;
    tbrGridTools: TTBToolbar;
    tbrLayoutTools: TTBToolbar;
    tbrTranslateTools: TTBToolbar;
    tbrAlignTools: TTBToolbar;
    tbrCurveEditTools: TTBToolbar;

    tbtArrowTool: TTBItem;
    tbtShapeTool: TTBItem;
    tbtZoomTool: TTBItem;
    tbtPanTool: TTBItem;
    tbtPolyLineTool: TTBItem;
    tbtPolygonTool: TTBItem;
    tbtConnectorTool: TTBItem;
    tbtRectTool: TTBItem;
    tbtEllipseTool: TTBItem;
    tbtTextTool: TTBItem;
    tbtPictureTool: TTBItem;

    TBItem1: TTBItem;
    TBItem2: TTBItem;
    TBItem3: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBItem4: TTBItem;
    TBItem5: TTBItem;
    TBItem6: TTBItem;
    TBItem7: TTBItem;
    TBItem8: TTBItem;
    TBItem9: TTBItem;
    TBItem10: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBItem11: TTBItem;
    TBItem12: TTBItem;
    TBItem13: TTBItem;
    TBSeparatorItem4: TTBSeparatorItem;
    TBItem14: TTBItem;
    TBControlItem3: TTBControlItem;
    TBItem15: TTBItem;
    TBItem16: TTBItem;
    TBItem17: TTBItem;
    TBItem18: TTBItem;
    TBItem19: TTBItem;
    TBControlItem4: TTBControlItem;
    TBSeparatorItem5: TTBSeparatorItem;
    TBSeparatorItem6: TTBSeparatorItem;
    TBItem20: TTBItem;
    TBItem21: TTBItem;
    TBItem22: TTBItem;
    TBItem23: TTBItem;
    TBItem24: TTBItem;
    TBItem25: TTBItem;
    TBItem26: TTBItem;
    TBItem27: TTBItem;
    TBSeparatorItem7: TTBSeparatorItem;
    TBSeparatorItem8: TTBSeparatorItem;
    TBSeparatorItem9: TTBSeparatorItem;
    TBItem28: TTBItem;
    TBItem29: TTBItem;
    TBControlItem1: TTBControlItem;
    TBItem30: TTBItem;
    TBItem31: TTBItem;
    TBItem32: TTBItem;
    TBItem33: TTBItem;
    TBItem34: TTBItem;
    TBItem35: TTBItem;
    TBItem36: TTBItem;
    TBItem37: TTBItem;
    TBItem38: TTBItem;
    TBItem39: TTBItem;
    TBItem40: TTBItem;
    TBItem41: TTBItem;
    TBItem42: TTBItem;
    TBItem43: TTBItem;
    TBItem44: TTBItem;
    TBItem45: TTBItem;
    TBItem46: TTBItem;
    TBItem47: TTBItem;
    TBSeparatorItem10: TTBSeparatorItem;
    TBItem48: TTBItem;
    TBItem49: TTBItem;

    tdDockUp: TTBDock;
    tdDockLeft: TTBDock;
    tdDockRight: TTBDock;
    tdDockBottom: TTBDock;
    TBToolbar1: TTBToolbar;
    TBItem50: TTBItem;
    TBItem51: TTBItem;
    TBItem52: TTBItem;
    TBItem53: TTBItem;
    acLeft: TAction;
    acRight: TAction;
    acUp: TAction;
    acDown: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbtToolClick(Sender: TObject);
    procedure tbrStdToolsClose(Sender: TObject);
    procedure cbActiveLayerChange(Sender: TObject);
    procedure cbActiveSchemeChange(Sender: TObject);
    procedure cbZoomExit(Sender: TObject);
    procedure cbZoomClick(Sender: TObject);
    procedure cbZoomKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acFileNewExecute(Sender: TObject);
    procedure acLayerNewExecute(Sender: TObject);
    procedure acLayerDeleteExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acSchemeDeleteExecute(Sender: TObject);
    procedure acSchemeNewExecute(Sender: TObject);
    procedure acFilePropertiesExecute(Sender: TObject);
    procedure acArrangeForwardOneExecute(Sender: TObject);
    procedure acArrangeBackOneExecute(Sender: TObject);
    procedure acArrangeToFrontExecute(Sender: TObject);
    procedure acArrangeToBackExecute(Sender: TObject);
    procedure acArrangeGroupExecute(Sender: TObject);
    procedure acArrangeUngroupExecute(Sender: TObject);
    procedure acEditDeleteExecute(Sender: TObject);
    procedure acDockerExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditPasteUpdate(Sender: TObject);
    procedure acAlignExecute(Sender: TObject);
    procedure acFilePreviewExecute(Sender: TObject);
    procedure acLibItemAddExecute(Sender: TObject);
    procedure acViewOptionsExecute(Sender: TObject);
    procedure acFilePrintExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acEditDuplicateExecute(Sender: TObject);
    procedure acZoomActualExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acTranslateRotateCWExecute(Sender: TObject);
    procedure acTranslateRotateCCWExecute(Sender: TObject);
    procedure acTranslateFlipHorzExecute(Sender: TObject);
    procedure acTranslateFlipVerticalExecute(Sender: TObject);
    procedure acFileImportExecute(Sender: TObject);
    procedure acFileExportExecute(Sender: TObject);
    procedure acGridShowExecute(Sender: TObject);
    procedure acGridPixelShowExecute(Sender: TObject);
    procedure acGridSnapExecute(Sender: TObject);
    procedure acLayerToFrontExecute(Sender: TObject);
    procedure acLayerToBackExecute(Sender: TObject);
    procedure acDebugPointsExecute(Sender: TObject);
    procedure acCurveJoinExecute(Sender: TObject);
    procedure acCurveBreakExecute(Sender: TObject);
    procedure acCurveCloseExecute(Sender: TObject);
    procedure acCurveToLineExecute(Sender: TObject);
    procedure acCurveToCurveExecute(Sender: TObject);
    procedure acCurveBreakApartExecute(Sender: TObject);
    procedure acCurveCombineExecute(Sender: TObject);
    procedure acCurveFlattenExecute(Sender: TObject);
    procedure acCurveConvertToCurveExecute(Sender: TObject);
    procedure acGridOptionsExecute(Sender: TObject);
    procedure acDockerPaletteExecute(Sender: TObject);
    procedure acEditCloneExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acDebugHistoryExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure panColorsResize(Sender: TObject);
    procedure sptColorsMoved(Sender: TObject);
    procedure dgColorsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgColorsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbrMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acLeftExecute(Sender: TObject);
    procedure acRightExecute(Sender: TObject);
    procedure acUpExecute(Sender: TObject);
    procedure acDownExecute(Sender: TObject);
  private
    { Private declarations }
    FLastFilename: string;
    FActiveFlex: TFlexPanel;
    FActiveFlexModified: boolean;
    FIniting: boolean;
    FIsFileLoading: boolean;
    FInspDataNeedUpdate: boolean;
    FInspPropsNeedUpdate: boolean;
    FFilerProgress: integer;
    FToolbarItems: TList;
    FColors: TList;
    FMainFrame: TFlexChildFrame;
    procedure RegisterExternalControls;
    function  ExecuteImportExportDialog(Kind: TFlexFileSupportKind): integer;
    function  GetToolScale: integer;
    procedure SetCurrentOptions(Flex: TFlexPanel;
      const Edited: TOptionEditPages = AllOptions);
    function  CreateToolWindow(ToolForm: TCustomForm;
      DockTo: TTBDock): TTBToolWindow;
    procedure ToolbarItemClick(Sender: TObject);
    procedure ToolWindowClose(Sender: TObject);
    procedure ToolWinNeedClose(Sender: TObject);
    procedure ToolWinPopupChange(Sender: TObject);
    procedure ControlPropChanged(Sender: TObject; Prop: TCustomProp);
    procedure CheckTools;
    procedure CheckToolbars;
    procedure CheckToolButtons(Sender: TObject);
    procedure CheckUpdates(Flex: TFlexPanel);
    procedure HistoryChange(Sender: TObject);
    procedure CustomColorsChange(Sender: TObject);
    procedure ActiveLibChange(Sender: TObject);
    //function  GetActiveFlex: TFlexPanel;
    procedure FlexEndSelUpdate(Sender: TObject);
    procedure UpdateToolWins(Flex: TFlexPanel);
    procedure UpdateLayers(Flex: TFlexPanel);
    procedure UpdateSchemes(Flex: TFlexPanel);
    procedure UpdateAllOptions(const Edited: TOptionEditPages = AllOptions);
    procedure FlexProgress(Sender: TObject; Progress: integer;
      Process: TFlexFilerProcess);
    function  GetInFilerProcess: boolean;
    procedure BeginFilerProcess;
    procedure EndFilerProcess;
    procedure ViewOptionsExecute(Page: TOptionEditPage = opDocument);
    procedure RefreshColors;
    function CheckModifiedFlex: bool;
    procedure MainFrameChange;
    procedure OpenFile(filename: string);
  public
    { Public declarations }
    procedure CreateDocument(const DocName: string);
    procedure ControlNotify(Sender: TObject; Control: TFlexControl;
      Notify: TFlexNotify);
    function  SaveChanges(AskName: boolean = true): boolean;
    property  ActiveFlex: TFlexPanel read FActiveFlex; //GetActiveFlex;
    property  InFilerProcess: boolean read GetInFilerProcess;
    property FileName: string read FLastFilename;
    function ShowModal(const filename: string): TModalResult; reintroduce;
  end;

var
  EditMainForm: TEditMainForm;

implementation

uses
  Consts, ColorComboEdit, ToolMngr,
  fInspector, fLibrary, fPreview, fDocProps, fUserData, fLayers,
  {$IFDEF DEBUG_HISTORY} fHistoryDbg, {$ENDIF}
  {$IFDEF DEBUG_POINTS} fPointsDbg, {$ENDIF}
  FlexHistory, FlexActions;

{$R *.DFM}

const
  SDeleteLayer  = 'Do you really want to delete layer %s?';
  SDeleteScheme = 'Do you really want to delete scheme %s?';

  DefaultPalette: array[0..52] of TColor = (
   TColor($000000),
   TColor($191919),
   TColor($333333),
   TColor($4C4C4C),
   TColor($666666),
   TColor($808080),
   TColor($999999),
   TColor($B2B2B2),
   TColor($E5E5E5),
   TColor($FFFFFF),
   TColor($000080),
   TColor($008000),
   TColor($008080),
   TColor($800000),
   TColor($800080),
   TColor($808000),
   TColor($808080),
   TColor($C0C0C0),
   TColor($0000FF),
   TColor($00FF00),
   TColor($00FFFF),
   TColor($FF0000),
   TColor($FF00FF),
   TColor($FFFF00),
   TColor($400000),
   TColor($A00000),
   TColor($004000),
   TColor($404000),
   TColor($804000),
   TColor($408000),
   TColor($C08000),
   TColor($FF8000),
   TColor($40FF00),
   TColor($80FF00),
   TColor($000040),
   TColor($400040),
   TColor($800040),
   TColor($400080),
   TColor($FF0080),
   TColor($004080),
   TColor($404080),
   TColor($C08080),
   TColor($FF8080),
   TColor($00FF80),
   TColor($80FF80),
   TColor($FFFF80),
   TColor($8000FF),
   TColor($0080FF),
   TColor($4080FF),
   TColor($8080FF),
   TColor($C080FF),
   TColor($FF80FF),
   TColor($80FFFF)
  );

procedure TEditMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FMainFrame <> nil then
  begin
    FMainFrame.CloseFlex();
    FreeAndNil(FMainFrame);
    MainFrameChange();
  end;
end;

function TEditMainForm.CheckModifiedFlex(): bool;
var ModalRes: integer;
begin
 Result := true;
 if (FMainFrame = nil) or not FMainFrame.Flex.Modified then Exit;

 ModalRes := MessageDlg('Сохранить изменения в файл "' + ExtractFileName(FMainFrame.Filename) + '"?',
   mtWarning, [mbYes, mbNo, mbCancel], 0);
 case ModalRes of
  mrYes:
    if not SaveChanges(false) then
     Result := false;
  mrNo:
    ;
  mrCancel:
    Result := false;
 end;
end;

procedure TEditMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckModifiedFlex();
end;

procedure TEditMainForm.FormCreate(Sender: TObject);
var List: TStringList;
    i, Index: integer;
    Item: TMenuItem;
begin
 FIniting := true;
 FMainFrame := nil;
 LoadFlexCursors;
 RegisterExternalControls;
 // Insert all toolbars to main menu
 FToolbarItems := TList.Create;
 List := TStringList.Create;
 try
  // Collect all toolbars
  for i:=0 to ComponentCount-1 do
   if Components[i] is TTBToolbar then
    List.AddObject(TTBToolbar(Components[i]).Caption, Components[i]);
  if List.Count > 0 then begin
   // Sort by captions
   List.Sort;
   // Place to main menu
   Index := miView.IndexOf(miToolBarsDelimiter);
   if Index < 0 then Index := 0;
   for i:=List.Count-1 downto 0 do begin
    Item := TMenuItem.Create(miView);
    Item.OnClick := ToolbarItemClick;
    Item.Caption := List[i];
    Item.Tag := integer(List.Objects[i]);
    miView.Insert(Index, Item);
    FToolbarItems.Add(Item);
   end;
  end else
   // No toolbars: Hide delimiter in main menu
   miToolBarsDelimiter.Visible := false;
 finally
  List.Free;
 end;
 {$IFDEF DEBUG_HISTORY}
 fmHistoryDebug := TfmHistoryDebug.Create(Self);
 fmHistoryDebug.BorderStyle := bsSizeToolWin;
 fmHistoryDebug.Align := alBottom;
 fmHistoryDebug.Parent := Self;
 fmHistoryDebug.Height := 200;
 fmHistoryDebug.sbrMain.Visible := false;
 fmHistoryDebug.Visible := true;
 {$ENDIF}
 // Setup custom color change event
 FColors := TList.Create;
 CustomColors.OnChange := CustomColorsChange;
 RefreshColors;
 CheckToolbars;
end;

procedure TEditMainForm.FormDestroy(Sender: TObject);
begin
 CustomColors.OnChange := Nil;
 FColors.Free;
 FToolbarItems.Free;
end;

procedure TEditMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    FMainFrame.VKMenuDown();
end;

procedure TEditMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key = VK_MENU then
   FMainFrame.VKMenuUp();
end;

procedure TEditMainForm.FormShow(Sender: TObject);
var Tlw1, Tlw2: TTBToolWindow;
begin
 if FIniting then begin
  // Show Inspector
  Tlw1 := CreateToolWindow(fmLayers, tdDockRight);
  // Show layer manager
  Tlw2 := CreateToolWindow(fmInspector, Nil);
  // Arrange
  if Assigned(Tlw2) then Tlw2.CurrentDock := tdDockRight;
  if Assigned(Tlw1) then Tlw1.DockPos := Tlw1.DockPos + Tlw1.Height - 110;
  // Init
  fmLibrary.OnLibChange := ActiveLibChange;
  FIniting := False;
  CheckTools;
 end;

 if FindCmdLineSwitch('start_with_blank') then
   CreateDocument('');
end;

procedure TEditMainForm.RegisterExternalControls;
var i: integer;
    B: TBitmap;
    TBItem: TTBItem;
    Hint: string;
begin
 if Length(RegisteredFlexControls) = 0 then exit;
 B := TBitmap.Create;
 try
  B.Width := imgToolIcons.Width;
  B.Height := imgToolIcons.Height;
  for i:=0 to High(RegisteredFlexControls) do begin
   FillRect(B.Canvas.Handle, Rect(0, 0, B.Width, B.Height),
     GetStockObject(WHITE_BRUSH) );
   Hint := '';
   if not RegisteredFlexControls[i].GetToolInfo(B, Hint) then continue;
   tbrTools.Images.AddMasked(B, B.TransparentColor);
   TBItem := TTBItem.Create(Self);
   TBItem.ImageIndex := tbrTools.Images.Count-1;
   TBItem.GroupIndex := 1;
   TBItem.Hint := Hint;
   TBItem.Tag := integer(RegisteredFlexControls[i]);
   TBItem.OnClick := tbtToolClick;
   tbrTools.Items.Add(TBItem);
  end;
 finally
  B.Free;
 end;
end;

procedure TEditMainForm.tbtToolClick(Sender: TObject);
begin
 if Sender is TTBCustomItem then
  TTBCustomItem(Sender).Checked := true;
  with FMainFrame.Flex do
  if Sender = tbtZoomTool then begin
   EditPointControl := Nil;
   CreatingControlClass := Nil;
   ToolMode := ftmZoom;
  end else
  if Sender = tbtPanTool then begin
   //EditPointControl := Nil;
   CreatingControlClass := Nil;
   ToolMode := ftmPan;
  end else
  if Sender = tbtShapeTool then begin
   CreatingControlClass := Nil;
   //if SelectedCount = 1 then EditPointControl := Selected[0];
   ToolMode := ftmPointEdit;
  end else begin
   if Sender = tbtArrowTool then begin
    CreatingControlClass := Nil;
    //EditPointControl := Nil;
   end else
   if Sender = tbtPolyLineTool then
    CreatingControlClass := TFlexCurve
   else
   if Sender = tbtPolygonTool then
    CreatingControlClass := TFlexRegularPolygon
   else
   if Sender = tbtConnectorTool then
    CreatingControlClass := TFlexConnector
   else
   if Sender = tbtRectTool then
    CreatingControlClass := TFlexBox
   else
   if Sender = tbtEllipseTool then
    CreatingControlClass := TFlexEllipse
   else
   if Sender = tbtTextTool then
    CreatingControlClass := TFlexText
   else
   if Sender = tbtPictureTool then
    CreatingControlClass := TFlexPicture
   else
   if TTBCustomItem(Sender).Tag <> 0 then
    CreatingControlClass := TFlexControlClass(TTBCustomItem(Sender).Tag);
   //if ToolMode in [ftmZoom, ftmZooming, ftmPan, ftmPanning] then
    ToolMode := ftmSelect;
  end;
 CheckTools;
end;

procedure TEditMainForm.ToolbarItemClick(Sender: TObject);
var i: integer;
    Item: TMenuItem;
    ToolBar: TTBToolbar;
begin
 for i:=0 to FToolbarItems.Count-1 do begin
  Item := TMenuItem(FToolbarItems[i]);
  if Item <> Sender then continue;
  ToolBar := TTBToolbar(Item.Tag);
  ToolBar.Visible := not ToolBar.Visible;
  break;
 end;
 CheckToolbars;
end;

procedure TEditMainForm.CheckToolbars;
var i: integer;
    Item: TMenuItem;
    ToolBar: TTBToolbar;
begin
 for i:=0 to FToolbarItems.Count-1 do begin
  Item := TMenuItem(FToolbarItems[i]);
  ToolBar := TTBToolbar(Item.Tag);
  Item.Checked := ToolBar.Visible;
 end;
end;

procedure TEditMainForm.tbrStdToolsClose(Sender: TObject);
begin
 CheckToolbars;
end;

procedure TEditMainForm.CheckToolButtons(Sender: TObject);
begin
 if Sender <> ActiveFlex then exit;
 with TFlexPanel(Sender) do
 case ToolMode of
  ftmSelect:
    if not Assigned(CreatingControlClass) then tbtArrowTool.Click;
  ftmPan:
    tbtPanTool.Click;
 end;
end;

procedure TEditMainForm.CheckTools;
var Flex: TFlexPanel;
    IsDoc, IsSel, IsSelMany, IsModified: boolean;
    IsSelGroup, IsSelCurve, IsLib, IsChecked: boolean;
    IsSelAllCurves: boolean;
    IsLastLayer, IsFirstLayer: boolean;
    SelCount, i, FigCount: integer;
    CurveCaps: TPathEditFuncs;

 function GetHistoryCaption(const Ident: string; Index: integer): string;
 var Action: THistoryAction;
 begin
  if Assigned(Flex) and
     (Index >= 0) and (Index < Flex.History.ActionCount) then begin
   Action := Flex.History[Index];
   while (Action is THistoryGroup) and
     (THistoryGroup(Action).ActionCount = 1) do
    Action := THistoryGroup(Action).Actions[0];
   Result := Ident + ' "' + Action.Caption + '"';
  end else
   Result := Ident;
 end;

begin
 Flex := ActiveFlex;
 // Flags
 if Assigned(Flex) then with Flex do begin
  IsDoc := True;
  SelCount := SelectedCount;
  IsSel := SelCount > 0;
  IsSelMany := SelCount > 1;
  //IsSelGroup := (SelCount = 1) and (Selected[0] is TFlexGroup);
  IsSelGroup := false;
  for i:=0 to SelCount-1 do begin
   IsSelGroup := Selected[i].IsUngroupable;
   if IsSelGroup then break;
  end;
  IsSelCurve := (SelCount = 1) and (Selected[0].PointCount > 0);
  if IsSelCurve {and Flex.IsEditPointsVisible} then begin
   if Flex.IsEditPointsVisible
    then CurveCaps := Flex.EditPointsCaps
    else CurveCaps := [];
   FigCount := Length(Selected[0].PointsInfo.Figures);
  end else begin
   CurveCaps := [];
   FigCount := 0;
  end;
  IsSelAllCurves := true;
  for i:=0 to SelCount-1 do
   if Selected[i].PointCount = 0 then begin
    IsSelAllCurves := false;
    break;
   end;
  IsModified := Flex.Modified;
  FActiveFlexModified := Flex.Modified;
  IsLastLayer := Layers.IndexOf(ActiveLayer) = Layers.Count-1;
  IsFirstLayer := Layers.IndexOf(ActiveLayer) = 0;
  acLayerDelete.Enabled := cbActiveLayer.Properties.Items.Count > 1;
  acSchemeDelete.Enabled := cbActiveScheme.Properties.Items.Count > 1;
 end else begin
  IsDoc := False;
  IsSel := False;
  IsSelMany := False;
  IsSelGroup := False;
  IsSelAllCurves := False;
  IsSelCurve := False;
  CurveCaps := [];
  FigCount := 0;
  IsModified := False;
  IsLastLayer := False;
  IsFirstLayer := False;
  cbActiveLayer.Properties.Items.Clear;
  cbActiveScheme.Properties.Items.Clear;
  acLayerDelete.Enabled := False;
  acSchemeDelete.Enabled := False;
  FActiveFlexModified := false;
 end;
 IsLib := Assigned(fmLibrary) and Assigned(fmLibrary.ActiveLibrary);
 IsChecked := false;
 // Draw tools
 for i:=0 to tbrTools.Items.Count-1 do with tbrTools.Items[i] do begin
  Enabled := IsDoc;
  if not IsDoc then
   Checked := False
  else
  if Checked then
   IsChecked := true;
 end;
 if IsDoc and not IsChecked then tbtArrowTool.Checked := true;
 // Grid
 acGridShow.Checked := EditOptions.ShowGrid;
 acGridPixelShow.Checked := EditOptions.ShowPixGrid;
 acGridSnap.Checked := EditOptions.SnapToGrid;
 // Dockers
 acDockerInspector.Checked := Assigned(FindToolParentContainer(fmInspector));
 acDockerLibrary.Checked := Assigned(FindToolParentContainer(fmLibrary));
 acDockerUserData.Checked := Assigned(FindToolParentContainer(fmUserData));
 acDockerLayers.Checked := Assigned(FindToolParentContainer(fmLayers));
 acDockerPalette.Checked := panColors.Visible; 
 // Edit
 acEditDelete.Enabled := IsSel;
 acEditCut.Enabled := IsSel;
 acEditCopy.Enabled := IsSel;
 acEditDuplicate.Enabled := IsSel;
 acEditClone.Enabled := IsSel {and
   (IsSelMany or not (Flex.Selected[0] is TFlexClone))};
 acEditUndo.Enabled := IsDoc and (Flex.History.ActionIndex >= 0);
 acEditRedo.Enabled := IsDoc and (Flex.History.ActionCount > 0) and
   (Flex.History.ActionIndex < Flex.History.ActionCount - 1);
 if IsDoc then begin
  acEditUndo.Hint := GetHistoryCaption('Undo', Flex.History.ActionIndex);
  acEditRedo.Hint := GetHistoryCaption('Redo', Flex.History.ActionIndex +1);
 end else begin
  acEditUndo.Hint := 'Undo';
  acEditRedo.Hint := 'Redo';
 end;
 acLibItemAdd.Enabled := IsSel and IsLib;
 // Layout
 cbActiveLayer.Enabled := IsDoc;
 cbActiveScheme.Enabled := IsDoc;
 acLayerNew.Enabled := IsDoc;
 acSchemeNew.Enabled := IsDoc;
 acLayerToFront.Enabled := IsDoc and not IsLastLayer;
 acLayerToBack.Enabled := IsDoc and not IsFirstLayer;
 // File
 acFileProperties.Enabled := IsDoc;
 acFilePreview.Enabled := IsDoc and not Assigned(fmPreview);
 acFileSave.Enabled := IsDoc and IsModified;
 acFileSaveAs.Enabled := IsDoc;
 acFilePrint.Enabled := IsDoc;
 acFileExport.Enabled := IsDoc;
 // Arrange
 acArrangeForwardOne.Enabled := IsSel;
 acArrangeBackOne.Enabled := IsSel;
 acArrangeToFront.Enabled := IsSel;
 acArrangeToBack.Enabled := IsSel;
 acArrangeGroup.Enabled := IsSelMany;
 acArrangeUngroup.Enabled := IsSelGroup;
 // Translate
 acTranslateRotateCW.Enabled := IsSel;
 acTranslateRotateCCW.Enabled := IsSel;
 acTranslateFlipHorz.Enabled := IsSel;
 acTranslateFlipVertical.Enabled := IsSel;
 // Curve edit
 acCurveJoin.Enabled := pfJoin in CurveCaps;
 acCurveBreak.Enabled := pfBreak in CurveCaps;
 acCurveClose.Enabled := pfClose in CurveCaps;
 acCurveToLine.Enabled := pfToLine in CurveCaps;
 acCurveToCurve.Enabled := pfToCurve in CurveCaps;
 acCurveFlatten.Enabled := IsSelCurve and
   Flex.Selected[0].PointsInfo.IsCurve;
 acCurveBreakApart.Enabled := FigCount > 1;
 acCurveCombine.Enabled := IsSelMany and IsSelAllCurves;
 acCurveConvertToCurve.Enabled := IsSel;
 // Align
 acAlignLeft.Enabled := IsSelMany;
 acAlignHCenter.Enabled := IsSelMany;
 acAlignRight.Enabled := IsSelMany;
 acAlignTop.Enabled := IsSelMany;
 acAlignVCenter.Enabled := IsSelMany;
 acAlignBottom.Enabled := IsSelMany;
 acAlignCenter.Enabled := IsSelMany;
 // Zoom
 cbZoom.Enabled := IsDoc;
 if cbZoom.Enabled
  then cbZoom.Text := IntToStr(Flex.Scale)+'%'
  else cbZoom.Text := '';
 acZoomIn.Enabled := IsDoc and (Flex.Scale < MaxScale);
 acZoomOut.Enabled := IsDoc and (Flex.Scale > MinScale);
 acZoomActual.Enabled := IsDoc and (Flex.Scale <> 100);
 // Move
 acUp.Enabled := IsSel;
 acDown.Enabled := IsSel;
 acLeft.Enabled := IsSel;
 acRight.Enabled := IsSel;
end;

function TEditMainForm.CreateToolWindow(ToolForm: TCustomForm;
  DockTo: TTBDock): TTBToolWindow;
var Cont: TToolContainer;
begin
 if Assigned(DockTo) and (DockTo.ToolbarCount > 0) then begin
  Cont := FindChildContainer(DockTo.Toolbars[0]);
  if Assigned(Cont) then begin
   Cont.InsertTool(ToolForm);
   Cont.ActivePageForm := ToolForm;
   Result := Nil;
   exit;
  end;
 end;
 Result := TTBToolWindow.Create(Self);
 with Result do begin
  Caption := '';
  CloseButtonWhenDocked := True;
  Width := 206;
  Height := 300;
  FloatingPosition := Self.ClientToScreen(Point(50, 50));
  Stretch := True;
  MinClientWidth := 100;
  //BorderStyle := bsNone;
  DragHandleStyle := dhDouble;
  //OnDockChanged := TBToolWindow1DockChanged;
  OnClose := ToolWindowClose;
  if Assigned(DockTo) then begin
   CurrentDock := DockTo
  end else begin
   Parent := Self;
   Floating := True;
  end;
  with TToolContainer.Create(Result) do begin
   Parent := Result;
   Caption := '';
   BorderStyle := bsNone;
   Show;
   OnNeedClose := ToolWinNeedClose;
   OnPopupChange := ToolWinPopupChange;
   if Assigned(ToolForm) then InsertTool(ToolForm);
  end;
 end;
end;

procedure TEditMainForm.ToolWindowClose(Sender: TObject);
var Container: TToolContainer;
begin
 Container := FindChildContainer(TWinControl(Sender));
 if Assigned(Container) then Container.RemoveAll;
 Sender.Free;
end;

procedure TEditMainForm.ToolWinNeedClose(Sender: TObject);
var Control: TWinControl;
begin
 Control := TWinControl(Sender).Parent;
 if Control is TTBToolWindow then TTBToolWindow(Control).Hide;
end;

procedure TEditMainForm.ToolWinPopupChange(Sender: TObject);
begin
 if not (csDestroying in ComponentState) then CheckTools;
end;

procedure TEditMainForm.acDockerExecute(Sender: TObject);
var ToolForm: TCustomForm;
    Container: TToolContainer;
begin
 if Sender = acDockerInspector then ToolForm := fmInspector else
 if Sender = acDockerLibrary   then ToolForm := fmLibrary else
 if Sender = acDockerUserData  then ToolForm := fmUserData else
 if Sender = acDockerLayers    then ToolForm := fmLayers else exit;
 Container := FindToolParentContainer(ToolForm);
 if not Assigned(Container) then
  CreateToolWindow(ToolForm, tdDockRight)
 else
  Container.RemoveTool(ToolForm);
 CheckTools;
end;

procedure TEditMainForm.acDockerPaletteExecute(Sender: TObject);
var Enabled: boolean;
begin
 Enabled := not acDockerPalette.Checked; // invert
 if panColors.Visible = Enabled then exit;
 if Enabled then begin
  // Show palette
  panColors.Visible := true;
  panColors.Left := Self.ClientWidth;
  sptColors.Visible := true;
  sptColors.Left := panColors.Left;
 end else begin
  // Hide palette
  panColors.Visible := false;
  sptColors.Visible := false;
 end;
 CheckTools;
end;

procedure TEditMainForm.acDownExecute(Sender: TObject);
begin
  if Assigned(ActiveFlex) then
    ActiveFlex.MoveSelected(0, PixelScaleFactor);
end;

procedure TEditMainForm.acLayerNewExecute(Sender: TObject);
var Layer: TFlexLayer;
begin
 if not Assigned(ActiveFlex) then exit;
 with ActiveFlex do begin
  Layer := Layers.New;
  ActiveLayer := Layer;
 end;
 fmInspector.Control := Layer;
end;

procedure TEditMainForm.acLayerDeleteExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 with ActiveFlex do
  if MessageDlg(Format(SDeleteLayer, [ActiveLayer.Name]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
   Layers.Remove(ActiveLayer);
end;

procedure TEditMainForm.acSchemeNewExecute(Sender: TObject);
var Scheme: TFlexScheme;
begin
 if not Assigned(ActiveFlex) then exit;
 with ActiveFlex do begin
  Scheme := TFlexScheme.Create(Schemes.Owner, Schemes, Nil); //Schemes.New;
  ActiveScheme := Scheme;
 end;
 fmInspector.Control := Scheme;
end;

procedure TEditMainForm.acSchemeDeleteExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 with ActiveFlex do
  if MessageDlg(Format(SDeleteScheme, [ActiveScheme.Name]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
   Schemes.Remove(ActiveScheme);
end;

procedure TEditMainForm.acFileNewExecute(Sender: TObject);
begin
 if CheckModifiedFlex() then
 begin
   FreeAndNil(FMainFrame);
   CreateDocument('');
 end;
end;

procedure TEditMainForm.MainFrameChange();
var Flex: TFlexPanel;
begin
 if Assigned(FMainFrame)
  then Flex := FMainFrame.Flex
  else Flex := Nil;
 if Assigned(Flex) and (csDestroying in Flex.ComponentState) then Flex := Nil;
 if Flex <> FActiveFlex then begin
  FActiveFlex := Flex;
  UpdateToolWins(Flex);
  UpdateLayers(Flex);
  UpdateSchemes(Flex);
  //SetCurrentOptions(Flex);
  CheckTools;
  {$IFDEF DEBUG_HISTORY}
  if Assigned(fmHistoryDebug) then fmHistoryDebug.Flex := Flex;
  {$ENDIF}
 end;
end;

procedure TEditMainForm.HistoryChange(Sender: TObject);
begin
 if Assigned(FActiveFlex) and (Sender = FActiveFlex.History) then begin
  CheckTools;
  {$IFDEF DEBUG_HISTORY}
  if Assigned(fmHistoryDebug) then fmHistoryDebug.HistoryChange(Sender);
  {$ENDIF}
 end;
end;

procedure TEditMainForm.UpdateToolWins(Flex: TFlexPanel);
begin
 if Assigned(fmInspector) then fmInspector.ActiveFlex := Flex;
 if Assigned(fmLibrary) then fmLibrary.ActiveFlex := Flex;
 if Assigned(fmUserData) then fmUserData.ActiveFlex := Flex;
 if Assigned(fmLayers) then fmLayers.ActiveFlex := Flex;
end;

procedure TEditMainForm.ControlPropChanged(Sender: TObject; Prop: TCustomProp);
var Panel: TFlexPanel;
begin
 if FIsFileLoading then exit;
 if Sender = fmInspector.Control then fmInspector.UpdateProps(Prop);
 if (Sender = fmUserData.SelControl) and Assigned(Prop) and
    (Prop = TFlexControl(Sender).UserData) then fmUserData.UpdateData;
 if Assigned(Sender) and (Sender is TFlexLayer) then begin
  if Assigned(fmLayers) then fmLayers.UpdateData(TFlexLayer(Sender));
  if Assigned(fmInspector) and Assigned(fmInspector.Control) then
   fmInspector.UpdateProps(fmInspector.Control.LayerProp);
 end;
 Panel := TFlexControl(Sender).Owner;
 if not (Panel.ToolMode in
   [ ftmResizing, ftmMoving, ftmSelecting, ftmZoom, ftmZooming,
     ftmMove, ftmMoving, ftmResize, ftmResizing, ftmCreating,
     ftmPointSelecting, ftmCurveMoving, ftmPan, ftmPanning ]) or
   ((Panel = ActiveFlex) and (Panel.Modified <> FActiveFlexModified)) then
  CheckTools;
end;

procedure TEditMainForm.ActiveLibChange(Sender: TObject);
begin
 CheckTools;
end;

procedure TEditMainForm.CreateDocument(const DocName: string);
begin
 if FMainFrame = nil then
 begin
   FMainFrame := TFlexChildFrame.Create(Application);
   FMainFrame.Align := alClient;
   FMainFrame.Parent := self;
 end;
 with FMainFrame do begin
  Flex.NewDocument();
  Flex.InDesign := true;
  Flex.History.DisableRecording;
  try
   Flex.OnNotify := ControlNotify;
   Flex.OnPropChanged := ControlPropChanged;
   Flex.OnEndSelectionUpdate := FlexEndSelUpdate;
   Flex.OnProgress := FlexProgress;
   Flex.OnToolMode := CheckToolButtons;
   Flex.History.OnChange := HistoryChange;
   Flex.PopupMenu := pmControl;
   if DocName = '' then begin
    Filename := 'Document' + IntToStr(Self.MDIChildCount+1);
    Flex.DocWidth := EditOptions.DocWidth;
    Flex.DocHeight := EditOptions.DocHeight;
    Flex.Modified := False;
    SetCurrentOptions(Flex);
   end else begin
    Caption := DocName;
   end;
   MainFrameChange();
  finally
   Flex.History.EnableRecording;
  end;
 end;
end;

procedure TEditMainForm.SetCurrentOptions(Flex: TFlexPanel;
  const Edited: TOptionEditPages);
begin
 if not Assigned(Flex) then exit;
 with EditOptions do begin
  if opDocument in Edited then begin
   //Flex.DocWidth := DocWidth;
   //Flex.DocHeight := DocHeight;
   Flex.AutoNameNumbs := DocAutoNameNumbs;
  end;
  if opGrid in Edited then begin
   Flex.ShowGrid := ShowGrid;
   Flex.SnapToGrid := SnapToGrid;
   Flex.SnapStyle := SnapStyle;
   Flex.ShowPixGrid := ShowPixGrid;
   Flex.GridStyle := GridStyle;
   Flex.GridColor := GridColor;
   Flex.GridPixColor := GridPixColor;
   Flex.GridHorizSize := GridHSize;
   Flex.GridVertSize := GridVSize;
   Flex.GridControl.HOffset := GridHOffset;
   Flex.GridControl.VOffset := GridVOffset;
   Flex.Refresh;
  end;
  if opConnectors in Edited then begin
   // Flex.ConnectorsMinGap := ConnectorsMinGap;
  end;
 end;
end;

function TEditMainForm.ShowModal(const filename: string): TModalResult;
begin
  if filename <> '' then
  begin
    OpenFile(filename);
  end
  else
  begin
    CreateDocument('');
    FMainFrame.Flex.Modified := true;
  end;

  Result := inherited ShowModal();
end;

procedure TEditMainForm.cbActiveLayerChange(Sender: TObject);
begin
 if Assigned(ActiveFlex) and (cbActiveLayer.ItemIndex >= 0) then
   with ActiveFlex do
   begin
    ActiveLayer :=
      TFlexLayer(cbActiveLayer.Properties.Items.Objects[cbActiveLayer.ItemIndex]);
    UnselectAll;
    fmInspector.Control := ActiveLayer;
   end;
end;

procedure TEditMainForm.cbActiveSchemeChange(Sender: TObject);
begin
 if Assigned(ActiveFlex) and (cbActiveScheme.ItemIndex >= 0) then
 begin
  ActiveFlex.ActiveScheme :=
   TFlexScheme(cbActiveScheme.Properties.Items.Objects[cbActiveScheme.ItemIndex]);
  fmInspector.Control := ActiveFlex.ActiveScheme;
 end;
end;

function TEditMainForm.SaveChanges(AskName: boolean = true): boolean;
var SaveName: string;
begin
 SaveName := FMainFrame.Filename;
 if AskName or FMainFrame.NewDocument then begin
  sd_Main.FileName := SaveName;
  Result := sd_Main.Execute;
  if not Result then exit;
  SaveName := sd_Main.FileName;
 end;
 BeginFilerProcess;
 try
  FMainFrame.UpdateScript();
  Result := FMainFrame.Flex.SaveToFile(SaveName);
  FMainFrame.Filename := SaveName;
  FLastFilename := SaveName;
 finally
  EndFilerProcess;
 end;
 CheckTools;
end;

procedure TEditMainForm.acFileSaveExecute(Sender: TObject);
begin
 SaveChanges(false);
end;

procedure TEditMainForm.acFileSaveAsExecute(Sender: TObject);
begin
 SaveChanges();
end;

function TEditMainForm.ExecuteImportExportDialog(
  Kind: TFlexFileSupportKind): integer;
var
  Dialog: TOpenDialog;
  i: integer;
  Extensions: TStringList;
  FilterStr, Ext: string;
begin
  Result := -1;
  case Kind of
    skImport: Dialog := od_Import;
    skExport: Dialog := sd_Export;
    else
      Exit;
  end;
  Extensions := TStringList.Create;
  try
    // Include all extensions that support necessary Kind
    for i:=0 to RegisteredFlexFileFormats.Count-1 do
      with RegisteredFlexFileFormats[i] do
        if Kind in Kinds then begin
          Extensions.AddObject(
            Format('%s (*.%s)|*.%1:s', [Description, Extension]),
            pointer(i));
        end;
    Extensions.Sort;
    // Add 'ALL' item
    if (Kind = skImport) and (Extensions.Count > 1) then begin
      FilterStr := '';
      for i:=0 to Extensions.Count-1 do begin
        Ext := Format('*.%s', [
          RegisteredFlexFileFormats[integer(Extensions.Objects[i])].Extension]);
        if i = 0 then
          FilterStr := Ext
        else
          FilterStr := Format('%s;%s', [FilterStr, Ext]);
      end;
      Extensions.InsertObject(0,
        Format('All (%s)|%0:s', [FilterStr]), pointer(-1));
    end;
    // Make filter string for dialog
    for i:=0 to Extensions.Count-1 do
      if i = 0 then
        FilterStr := Extensions[i]
      else
        FilterStr := Format('%s|%s', [FilterStr, Extensions[i]]);
    Dialog.Filter := FilterStr;
    // Execute dialog
    if Dialog.Execute then begin
      Result := integer(Extensions.Objects[Dialog.FilterIndex - 1]);
      if Result < 0 then begin
        // Define filter index by extension
        Ext := ExtractFileExt(Dialog.FileName);
        if (Length(Ext) > 1) and (Ext[1] = '.') then Delete(Ext, 1, 1);
        for i:=0 to RegisteredFlexFileFormats.Count-1 do
          with RegisteredFlexFileFormats[i] do
            if (Kind in Kinds) and (CompareText(Extension, Ext) = 0) then begin
              Result := i;
              Break;
            end;
      end;
    end;
  finally
    Extensions.Free;
  end;
end;

procedure TEditMainForm.acFileImportExecute(Sender: TObject);
var Index: integer;
    FxdFilename: string;
begin
 Index := ExecuteImportExportDialog(skImport);
 if Index < 0 then exit;
 // Create form for document
 FxdFilename := ChangeFileExt(od_Import.Filename, '.fxd');
 CreateDocument(ExtractFilename(FxdFilename));
 try
  if not Assigned(FMainFrame) then exit;
  BeginFilerProcess;
  try
   FIsFileLoading := true;
   RegisteredFlexFileFormats.ImportFromFile(Index, FMainFrame.Flex,
     od_Import.FileName);
   FMainFrame.Filename := FxdFilename;
   // Update tools
   SetCurrentOptions(FMainFrame.Flex, AllOptions);
   fmUserData.UpdateData;
   UpdateLayers(FMainFrame.Flex);
   UpdateSchemes(FMainFrame.Flex);
   fmLibrary.FlexSelectionChange;
   fmInspector.UpdateData;
   fmInspector.UpdateProps(Nil);
  finally
   FIsFileLoading := false;
   EndFilerProcess;
  end;
  sd_Main.FileName := FxdFilename;
 except
  FreeAndNil(FMainFrame);
  MainFrameChange();
  raise;
 end;
 CheckTools;
end;

procedure TEditMainForm.acFileExportExecute(Sender: TObject);
var Flex: TFlexPanel;
    Index: integer;
begin
 Flex := ActiveFlex;
 if not Assigned(Flex) then exit;
 Index := ExecuteImportExportDialog(skExport);
 if Index >= 0 then
  RegisteredFlexFileFormats.ExportToFile(Index, Flex, sd_Export.FileName);
end;

procedure TEditMainForm.acFileOpenExecute(Sender: TObject);
begin
  if not od_Main.Execute then exit;

  OpenFile(od_Main.Filename);
end;

procedure TEditMainForm.OpenFile(filename: string);
begin
 // Search in already opened documents
 if FMainFrame = nil then
   CreateDocument('');
 // Create form for document
 CreateDocument(ExtractFilename(filename));
 FLastFilename := filename;
 try
  if not Assigned(FMainFrame) then exit;
  BeginFilerProcess;
  try
   FIsFileLoading := true;
   FMainFrame.Flex.LoadFromFile(filename);
   FMainFrame.Script := FMainFrame.Flex.Script.Text;
   FMainFrame.Filename := filename;
   // Update tools
   SetCurrentOptions(FMainFrame.Flex, AllOptions);
   fmUserData.UpdateData;
   UpdateLayers(FMainFrame.Flex);
   UpdateSchemes(FMainFrame.Flex);
   fmLibrary.FlexSelectionChange;
   fmInspector.UpdateData;
   fmInspector.UpdateProps(Nil);
  finally
   FIsFileLoading := false;
   EndFilerProcess;
  end;
  sd_Main.FileName := filename;
 except
  FreeAndNil(FMainFrame);
  MainFrameChange();
  raise;
 end;
 CheckTools;
end;

procedure TEditMainForm.acFilePrintExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) or not pd_Main.Execute then exit;
 ActiveFlex.Print(Printer, True, pd_Main.PrintRange = prSelection);
end;

procedure TEditMainForm.ViewOptionsExecute(Page: TOptionEditPage = opDocument);
var fmOptions: TfmOptions;
begin
 fmOptions := TfmOptions.Create(Application);
 try
  fmOptions.EditOptions := EditOptions;
  fmOptions.Page := Page;
  if fmOptions.ShowModal = mrOk then begin
   Move(fmOptions.EditOptions, EditOptions, SizeOf(EditOptions));
   UpdateAllOptions(fmOptions.Edited);
  end;
 finally
  fmOptions.Free;
 end;
end;

procedure TEditMainForm.acViewOptionsExecute(Sender: TObject);
begin
 ViewOptionsExecute;
end;

procedure TEditMainForm.acFilePropertiesExecute(Sender: TObject);
var Flex: TFlexPanel;
begin
 Flex := ActiveFlex;
 if not Assigned(Flex) then exit;
 fmDocProps := TfmDocProps.Create(Application);
 with fmDocProps do
 try
  Tag := integer(Flex);
  if ShowModal = mrOk then Flex.Refresh;
 finally
  fmDocProps.Free;
 end;
end;

procedure TEditMainForm.UpdateLayers(Flex: TFlexPanel);
var StrList: TStringList;
    i: integer;
begin
 if not Assigned(Flex) then exit;
 StrList := Nil;
 cbActiveLayer.Properties.Items.BeginUpdate;
 try
  StrList := TStringList.Create;
  for i:=0 to Flex.Layers.Count-1 do
   StrList.AddObject(Flex.Layers[i].Name, Flex.Layers[i]);
  StrList.Sort;
  cbActiveLayer.ItemIndex := -1;
  cbActiveLayer.Properties.Items.Assign(StrList);
  cbActiveLayer.ItemIndex := StrList.IndexOfObject(Flex.ActiveLayer);
  cbActiveLayer.Refresh;
 finally
  cbActiveLayer.Properties.Items.EndUpdate;
  StrList.Free;
 end;
 fmLayers.UpdateData;
end;

procedure TEditMainForm.UpdateSchemes(Flex: TFlexPanel);
var StrList: TStringList;
    i: integer;
begin
 if not Assigned(Flex) then exit;
 StrList := Nil;
 cbActiveScheme.Properties.Items.BeginUpdate;
 try
  StrList := TStringList.Create;
  for i:=0 to Flex.Schemes.Count-1 do
   StrList.AddObject(Flex.Schemes[i].Name, Flex.Schemes[i]);
  StrList.Sort;
  cbActiveScheme.Properties.Items.Assign(StrList);
  cbActiveScheme.ItemIndex := StrList.IndexOfObject(Flex.ActiveScheme);
 finally
  cbActiveScheme.Properties.Items.EndUpdate;
  StrList.Free;
 end;
end;

procedure TEditMainForm.UpdateAllOptions(const Edited: TOptionEditPages);
begin
  SetCurrentOptions(FMainFrame.Flex, Edited);
end;

procedure TEditMainForm.CheckUpdates(Flex: TFlexPanel);
begin
 if Flex.SelectionUpdateCounter = 0 then begin
  if FInspDataNeedUpdate then begin
   fmInspector.UpdateData;
   FInspDataNeedUpdate := False;
  end;
  if FInspPropsNeedUpdate then begin
   fmInspector.UpdateProps(Nil);
   FInspDataNeedUpdate := False;
  end;
 end;
end;

procedure TEditMainForm.FlexEndSelUpdate(Sender: TObject);
begin
 if Sender = ActiveFlex then CheckUpdates(TFlexPanel(Sender));
end;

procedure TEditMainForm.ControlNotify(Sender: TObject;
  Control: TFlexControl; Notify: TFlexNotify);
var Flex: TFlexPanel;
    NeedUpdate: boolean;
begin
 if FIsFileLoading or (csDestroying in ComponentState) or
    not Assigned(Sender) or not (Sender is TFlexPanel) then exit;
 Flex := TFlexPanel(Sender);
 if Flex <> ActiveFlex then exit;
 if Notify in [ fnName, fnCreated, fnDestroyed, fnOrder ] then begin
  {if Flex.SelectionUpdateCounter = 0 then fmInspector.UpdateData; }
  FInspDataNeedUpdate := True;
 end;
 NeedUpdate := false;
 case Notify of
  fnDestroyed:
    begin
     if Control = fmInspector.Control then fmInspector.Control := Nil;
     fmUserData.UpdateData;
    end;
  fnName:
    if Assigned(Control) then
     if Control is TFlexLayer then UpdateLayers(Flex) else
     if Control is TFlexScheme then UpdateSchemes(Flex);
  fnSelect:
    begin
     if Flex.SelectedCount = 1
      then fmInspector.Control := Flex.Selected[0]
      else fmInspector.Control := Nil;
     fmLibrary.FlexSelectionChange;
     fmUserData.UpdateData;
     if Flex.HandleAllocated then Windows.SetFocus(Flex.Handle);
     sbrMain.SimpleText := 'Selected: '+IntToStr(Flex.SelectedCount);
     NeedUpdate := true;
    end;
  fnLayers:
    UpdateLayers(Flex);
  fnSchemes:
    begin
     UpdateSchemes(Flex);
     FInspDataNeedUpdate := True;
     FInspPropsNeedUpdate := True;
   {  fmInspector.UpdateData;
     fmInspector.UpdateProps(Nil); }
    end;
  fnRect:
    exit;
  fnChange:
    CheckTools;
  fnScale:
    begin
     CheckTools;
     exit;
    end;
 end;
 if NeedUpdate or not (Flex.ToolMode in
   [ ftmResizing, ftmMoving, ftmSelecting, ftmZoom, ftmZooming,
     ftmMove, ftmMoving, ftmResize, ftmResizing, ftmCreating,
     {ftmPointSelecting, }ftmCurveMoving, ftmPan, ftmPanning ]) then begin
  CheckUpdates(Flex);
  CheckTools;
 end;
end;

procedure TEditMainForm.BeginFilerProcess;
begin
 Screen.Cursor := crHourGlass;
 sbrMain.SimplePanel := False;
end;

procedure TEditMainForm.EndFilerProcess;
begin
 Screen.Cursor := crDefault;
 sbrMain.SimplePanel := True;
end;

function TEditMainForm.GetInFilerProcess: boolean;
begin
 Result := not sbrMain.SimplePanel;
end;

procedure TEditMainForm.FlexProgress(Sender: TObject; Progress: integer;
  Process: TFlexFilerProcess);
begin
 if not InFilerProcess then exit;
 FFilerProgress := Progress;
 sbrMain.Refresh;
end;

procedure TEditMainForm.sbrMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var R: TRect;
begin
 if Panel <> sbrMain.Panels[0] then exit;
 R := Rect;
 InflateRect(R, -1, -1);
 R.Right := R.Left + Round(FFilerProgress / 100 * (R.Right - R.Left)); 
 with sbrMain.Canvas do begin
  Brush.Style := bsSolid;
  Brush.Color := clNavy;
  FillRect(R);
 end;
end;

procedure TEditMainForm.acArrangeForwardOneExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.ForwardOne;
end;

procedure TEditMainForm.acArrangeBackOneExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.BackOne;
end;

procedure TEditMainForm.acArrangeToFrontExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.ToFront;
end;

procedure TEditMainForm.acArrangeToBackExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.ToBack;
end;

procedure TEditMainForm.acArrangeGroupExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Group;
end;

procedure TEditMainForm.acArrangeUngroupExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Ungroup;
end;

procedure TEditMainForm.acEditCopyExecute(Sender: TObject);
begin
 if FMainFrame <> nil then
   FMainFrame.Copy;
end;

procedure TEditMainForm.acEditCutExecute(Sender: TObject);
begin
 if FMainFrame <> nil then
   FMainFrame.Cut;
end;

procedure TEditMainForm.acEditPasteExecute(Sender: TObject);
var usePoint: bool;
begin
 if FMainFrame = nil then Exit;

 usePoint := (Sender is TAction) and (TAction(Sender).ActionComponent = Paste2);
 FMainFrame.Paste(usePoint);
end;

procedure TEditMainForm.acEditPasteUpdate(Sender: TObject);
begin
 acEditPaste.Enabled :=
   Assigned(ActiveFlex) and ActiveFlex.PasteAvailable; //Clipboard.HasFormat(CF_FLEXDOC);
end;

procedure TEditMainForm.acAlignExecute(Sender: TObject);
var Align: TFlexAlign;
begin
 if Sender = acAlignLeft     then Align := faLeft else
 if Sender = acAlignHCenter  then Align := faHCenter else
 if Sender = acAlignRight    then Align := faRight else
 if Sender = acAlignTop      then Align := faTop else
 if Sender = acAlignVCenter  then Align := faVCenter else
 if Sender = acAlignBottom   then Align := faBottom else
 if Sender = acAlignCenter   then Align := faCenter
                             else Align := faLeft;
 if Assigned(ActiveFlex) then ActiveFlex.AlignSelected(Align);
end;

procedure TEditMainForm.acFilePreviewExecute(Sender: TObject);
var WasModified: boolean;
begin
 fmPreview := TfmPreview.Create(Nil);
 try
  BeginFilerProcess;
  try
   FMainFrame.UpdateScript();
   WasModified := ActiveFlex.Modified;
   fmPreview.Flex.OnProgress := FlexProgress;
   fmPreview.Flex.Assign(ActiveFlex);
   fmPreview.Flex.OnProgress := Nil;
   ActiveFlex.Modified := WasModified;
  finally
   EndFilerProcess;
  end;
  CheckTools;
  fmPreview.Caption := fmPreview.Caption + ' ['+ActiveMDIChild.Caption+']';
  fmPreview.ShowModal;
 finally
  fmPreview.Free;
  fmPreview := Nil;
  CheckTools;
 end;
end;

procedure TEditMainForm.acLibItemAddExecute(Sender: TObject);
begin
 if Assigned(fmLibrary) then fmLibrary.acLibAddItem.Execute;
end;

procedure TEditMainForm.acRightExecute(Sender: TObject);
begin
  if Assigned(ActiveFlex) then
    ActiveFlex.MoveSelected(PixelScaleFactor, 0);
end;

procedure TEditMainForm.acZoomInExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then with ActiveFlex do Zoom(Scale * 2, Nil);
end;

procedure TEditMainForm.acZoomOutExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then with ActiveFlex do Zoom(Scale div 2, Nil);
end;

procedure TEditMainForm.acZoomActualExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then with ActiveFlex do Zoom(100, Nil);
end;

function TEditMainForm.GetToolScale: integer;
var i, Len: integer;
    s: string;
begin
 Result := 100;
 if not Assigned(ActiveFlex) then exit;
 s := cbZoom.Text;
 Len := Length(s);
 for i:=1 to Len+1 do
  if (i > Len) or (s[i] < '0') or (s[i] > '9') then begin
   Result := StrToIntDef(copy(s, 1, i-1), -1);
   if Result < 0 then Result := ActiveFlex.Scale;
   break;
  end;
end;

procedure TEditMainForm.cbZoomExit(Sender: TObject);
var NewScale: integer;
begin
 NewScale := GetToolScale;
 if Assigned(ActiveFlex) then with ActiveFlex do
  if NewScale <> Scale then Zoom(NewScale, Nil);
end;

procedure TEditMainForm.cbZoomClick(Sender: TObject);
var NewScale: integer;
begin
 NewScale := GetToolScale;
 if Assigned(ActiveFlex) then with ActiveFlex do
  if NewScale <> Scale then Zoom(NewScale, Nil);
end;

procedure TEditMainForm.cbZoomKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_RETURN then
  cbZoomClick(cbZoom)
 else
 if Key = VK_ESCAPE then
  CheckTools;
end;

procedure TEditMainForm.acEditCloneExecute(Sender: TObject);
var Shift: TPoint;
    SelList: TList;
    i: integer;
begin
 if not Assigned(ActiveFlex) then exit;
 SelList := Nil;
 with ActiveFlex do
 try
  Shift := Point(EditOptions.ShiftX, EditOptions.ShiftY);
  if EditOptions.DupRandom then begin
   Randomize;
   Shift.X := Round(Random * 2*Shift.X) - Shift.X;
   Shift.Y := Round(Random * 2*Shift.Y) - Shift.Y;
   SelList := TList.Create;
   for i:=0 to SelectedCount-1 do SelList.Add(Selected[i]);
  end;
  CloneSelected(Shift.X, Shift.Y);
  if Assigned(SelList) then begin
   BeginSelectionUpdate;
   try
    UnselectAll;
    for i:=0 to SelList.Count-1 do Select(TFlexControl(SelList[i]));
   finally
    EndSelectionUpdate;
   end;
  end;
 finally
  SelList.Free;
 end;
end;

procedure TEditMainForm.acEditDeleteExecute(Sender: TObject);
begin
  if Assigned(FMainFrame) then
    FMainFrame.Del();
end;

procedure TEditMainForm.acEditDuplicateExecute(Sender: TObject);
var Shift: TPoint;
    SelList: TList;
    i: integer;
begin
 if not Assigned(ActiveFlex) then exit;
 SelList := Nil;
 with ActiveFlex do
 try
  Shift := Point(EditOptions.ShiftX, EditOptions.ShiftY);
  if EditOptions.DupRandom then begin
   Randomize;
   Shift.X := Round(Random * 2*Shift.X) - Shift.X;
   Shift.Y := Round(Random * 2*Shift.Y) - Shift.Y;
   SelList := TList.Create;
   for i:=0 to SelectedCount-1 do SelList.Add(Selected[i]);
  end;
  Duplicate(Shift.X, Shift.Y);
  if Assigned(SelList) then begin
   BeginSelectionUpdate;
   try
    UnselectAll;
    for i:=0 to SelList.Count-1 do Select(TFlexControl(SelList[i]));
   finally
    EndSelectionUpdate;
   end;
  end;
 finally
  SelList.Free;
 end;
end;

procedure TEditMainForm.acTranslateRotateCWExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Rotate(-90, False);
end;

procedure TEditMainForm.acUpExecute(Sender: TObject);
begin
  if Assigned(ActiveFlex) then
    ActiveFlex.MoveSelected(0, -PixelScaleFactor);
end;

procedure TEditMainForm.acTranslateRotateCCWExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Rotate(90, False);
end;

procedure TEditMainForm.acTranslateFlipHorzExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Rotate(0, True);
end;

procedure TEditMainForm.acTranslateFlipVerticalExecute(Sender: TObject);
begin
 if Assigned(ActiveFlex) then ActiveFlex.Rotate(180, True);
end;

procedure TEditMainForm.acGridShowExecute(Sender: TObject);
begin
 EditOptions.ShowGrid := not EditOptions.ShowGrid;
 UpdateAllOptions;
 CheckTools;
end;

procedure TEditMainForm.acGridPixelShowExecute(Sender: TObject);
begin
 EditOptions.ShowPixGrid := not EditOptions.ShowPixGrid;
 UpdateAllOptions;
 CheckTools;
end;

procedure TEditMainForm.acGridSnapExecute(Sender: TObject);
begin
 EditOptions.SnapToGrid := not EditOptions.SnapToGrid;
 UpdateAllOptions;
 CheckTools;
end;

procedure TEditMainForm.acLayerToFrontExecute(Sender: TObject);
var Index: integer;
begin
 if Assigned(ActiveFlex) then with ActiveFlex do begin
  Index := Layers.IndexOf(ActiveLayer);
  if Index < Layers.Count-1 then Layers.ChangeOrder(Index, Index+1);
 end;
end;

procedure TEditMainForm.acLeftExecute(Sender: TObject);
begin
  if Assigned(ActiveFlex) then
    ActiveFlex.MoveSelected(-PixelScaleFactor, 0);
end;

procedure TEditMainForm.acLayerToBackExecute(Sender: TObject);
var Index: integer;
begin
 if Assigned(ActiveFlex) then with ActiveFlex do begin
  Index := Layers.IndexOf(ActiveLayer);
  if Index > 0 then Layers.ChangeOrder(Index, Index-1);
 end;
end;

procedure TEditMainForm.acFileExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TEditMainForm.acDebugPointsExecute(Sender: TObject);
begin
 {$IFDEF DEBUG_POINTS}
 ShowPointsDebug;
 {$ENDIF}
end;

procedure TEditMainForm.acCurveJoinExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.EditPoints(pfJoin);
 CheckTools;
end;

procedure TEditMainForm.acCurveBreakExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.EditPoints(pfBreak);
 CheckTools;
end;

procedure TEditMainForm.acCurveCloseExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.EditPoints(pfClose);
 CheckTools;
end;

procedure TEditMainForm.acCurveToLineExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.EditPoints(pfToLine);
 CheckTools;
end;

procedure TEditMainForm.acCurveToCurveExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.EditPoints(pfToCurve);
 CheckTools;
end;

procedure TEditMainForm.acCurveFlattenExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 with ActiveFlex do FlattenSelected((Scale/100) * (1/PixelScaleFactor));
 CheckTools;
end;

procedure TEditMainForm.acCurveBreakApartExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.BreakApartSelected;
 CheckTools;
end;

procedure TEditMainForm.acCurveCombineExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.CombineSelected;
 CheckTools;
end;

procedure TEditMainForm.acCurveConvertToCurveExecute(Sender: TObject);
begin
 if not Assigned(ActiveFlex) then exit;
 ActiveFlex.ConvertSelectedToCurves;
end;

procedure TEditMainForm.acGridOptionsExecute(Sender: TObject);
begin
 ViewOptionsExecute(opGrid);
end;

procedure TEditMainForm.RefreshColors;
var i, j, Color: integer;
    s: string;
    ARows, ACols: integer;
begin
 FColors.Clear;
 // Add transparent color
 FColors.Add(pointer(clNone));
 // Add default colors
 for i:=Low(DefaultPalette) to High(DefaultPalette) do
  FColors.Add(pointer(ColorToRGB(DefaultPalette[i])));
 // Add custom colors
 for i:=0 to CustomColors.Count-1 do begin
  s := CustomColors[i];
  j := Pos('=', s);
  if j = 0 then continue;
  Color := StrToIntDef('$' + copy(s, j+1, Length(s)-j), -1);
  if Color = -1 then continue;
  FColors.Add(pointer(ColorToRGB(Color)));
 end;
 // Add default (color edit) color
 FColors.Add(pointer(clDefault));
 // Rebuild color grid
 with dgColors do begin
  RowCount := FColors.Count;
  i := (DefaultRowHeight + GridLineWidth);
  if i > 0
   then ARows := Height div i
   else ARows := 0;
  if ARows > 0
   then ACols := FColors.Count div ARows + byte(FColors.Count mod ARows <> 0)
   else ACols := 0;
  RowCount := ARows;
  ColCount := ACols;
  Invalidate;
 end;
end;

procedure TEditMainForm.CustomColorsChange(Sender: TObject);
begin
 RefreshColors;
end;

procedure TEditMainForm.dgColorsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var Index: integer;
    Color: TColor;
begin
 Index := ACol * dgColors.RowCount + ARow;
 with dgColors.Canvas do begin
  if Index < FColors.Count then begin
   Pen.Color := clBlack;
   Pen.Style := psSolid;
   Brush.Style := bsClear;
   {$IFDEF VER120} // {FG_D4}
   Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
   {$ELSE}
   Rectangle(Rect);
   {$ENDIF}
   InflateRect(Rect, -1, -1);
   Color := TColor(FColors[Index]);
   case Color of
    clNone:
      with Rect do begin
       Brush.Color := clWhite;
       Brush.Style := bsSolid;
       FillRect(Rect);
       MoveTo(Left, Top);
       LineTo(Right, Bottom);
       MoveTo(Right, Top-1);
       LineTo(Left-1, Bottom);
      end;
    clDefault:
      begin
       Brush.Color := clWhite;
       Brush.Style := bsSolid;
       FillRect(Rect);
       InflateRect(Rect, -4, 0);
       dec(Rect.Bottom, 2);
       Rect.Top := Rect.Bottom - 2;
       Brush.Color := clBlack;
       FillRect(Rect);
      end;
    else
      begin
       Brush.Color := ColorToRGB(Color);
       Brush.Style := bsSolid;
       FillRect(Rect);
      end;
   end;
  end;
 end;
end;

procedure TEditMainForm.panColorsResize(Sender: TObject);
begin
 RefreshColors;
end;

procedure TEditMainForm.sptColorsMoved(Sender: TObject);
begin
 RefreshColors;
end;

procedure TEditMainForm.dgColorsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Flex: TFlexPanel;
    Index, i: integer;
    ACol, ARow: integer;
    Color: TColor;
    Control: TFlexControl;
    Brush: TBrushProp;
    Pen: TPenProp;
    {$IFDEF USE_FLEXPLUS}
    BrushPlus: TBrushPlusProp;
    PenPlus: TPenPlusProp;
    {$ENDIF}
    PassRec: TPassControlRec;
begin
 dgColors.MouseToCell(X, Y, ACol, ARow);
 Index := ACol * dgColors.RowCount + ARow;
 if (Index >= 0) and (Index < FColors.Count)
  then Color := TColor(FColors[Index])
  else exit;
 Flex := ActiveFlex;
 if not Assigned(Flex) or (Flex.SelectedCount = 0) then exit;
 if Color = clDefault then begin
  // Define initial color
  Color := clBlack;
  for i:=0 to Flex.SelectedCount-1 do begin
   Control := Flex.Selected[i];
   FirstControl(Control, PassRec);
   while Assigned(Control) do begin
    Brush := Control.Props['Brush'] as TBrushProp;
    Pen := Control.Props['Pen'] as TPenProp;
    if (Button = mbLeft) and Assigned(Brush) and
      (Brush.Method = bmHatch) then begin
     // Get color
     Color := Brush.Color;
     if Brush.Style <> bsClear then break;
    end else
    if (Button = mbRight) and Assigned(Pen) then begin
     Color := Pen.Color;
     if Pen.Style <> psClear then break;
    end;
    {$IFDEF USE_FLEXPLUS}
    if not Assigned(Brush) then begin
     BrushPlus := Control.Props['BrushPlus'] as TBrushPlusProp;
     if (Button = mbLeft) and Assigned(BrushPlus) and
        (BrushPlus.Method in [bpmClear, bpmSolid, bpmHatch]) then begin
      // Get color
      Color := BrushPlus.Color;
      if (Color <> clNone) and (BrushPlus.Method <> bpmClear) then break;
     end;
    end;
    if not Assigned(Pen) then begin
     PenPlus := Control.Props['PenPlus'] as TPenPlusProp;
     if (Button = mbRight) and Assigned(PenPlus) then begin
      // Get color
      Color := PenPlus.Color;
      if (Color <> clNone) and (PenPlus.Method <> ppmClear) then break;
     end;
    end;
    {$ENDIF}
    // Next sub-control
    Control := NextControl(PassRec);
   end;
   ClosePassRec(PassRec);
  end;

  cd_Palette.Color := dxColorToAlphaColor(Color);
  // Update custom colors
  cd_Palette.CustomColors.Assign(CustomColors);
  if cd_Palette.Execute then begin
   CustomColors.Assign(cd_Palette.CustomColors);
   Color := dxAlphaColorToColor(cd_Palette.Color);
  end else
   exit;
 end;
 Flex.History.BeginPanelGroup(TPanelColorHistoryGroup);
 try
  for i:=0 to Flex.SelectedCount-1 do begin
   Control := Flex.Selected[i];
   FirstControl(Control, PassRec);
   while Assigned(Control) do begin
    Brush := Control.Props['Brush'] as TBrushProp;
    Pen := Control.Props['Pen'] as TPenProp;
    if (Button = mbLeft) and Assigned(Brush) then begin
     // Fill
     Brush.Method := bmHatch;
     if Color = clNone then
      Brush.Style := bsClear
     else begin
      Brush.Style := bsSolid;
      Brush.Color := ColorToRGB(Color);
     end;
    end else
    if (Button = mbRight) and Assigned(Pen) then begin
     // Outline
     if Color = clNone then
      Pen.Style := psClear
     else begin
      Pen.Style := psSolid;
      Pen.Color := ColorToRGB(Color);
     end;
    end;
    {$IFDEF USE_FLEXPLUS}
    if not Assigned(Brush) then begin
     BrushPlus := Control.Props['BrushPlus'] as TBrushPlusProp;
     if (Button = mbLeft) and Assigned(BrushPlus) then begin
      // Fill
      if Color = clNone then
       BrushPlus.Method := bpmClear
      else begin
       BrushPlus.Method := bpmSolid;
       BrushPlus.Color := Color;
      end;
     end;
    end;
    if not Assigned(Pen) then begin
     PenPlus := Control.Props['PenPlus'] as TPenPlusProp;
     if (Button = mbRight) and Assigned(PenPlus) then begin
      // Outline
      if Color = clNone then
       PenPlus.Method := ppmClear
      else begin
       PenPlus.Method := ppmDash;
       PenPlus.Color := Color;
      end;
     end;
    end;
    {$ENDIF}
    // Next sub-control
    Control := NextControl(PassRec);
   end;
   ClosePassRec(PassRec);
  end;
 finally
  Flex.History.EndPanelGroup(TPanelColorHistoryGroup);
 end;
end;

procedure TEditMainForm.acEditUndoExecute(Sender: TObject);
begin
 if Assigned(FMainFrame) then
   FMainFrame.Undo();

 {$IFDEF DEBUG_HISTORY}
 if Assigned(fmHistoryDebug) then
   fmHistoryDebug.HistoryChange(ActiveFlex.History);
 {$ENDIF} 
end;

procedure TEditMainForm.acEditRedoExecute(Sender: TObject);
begin
 if Assigned(FMainFrame) then
   FMainFrame.Redo();

 {$IFDEF DEBUG_HISTORY}
 if Assigned(fmHistoryDebug) then
   fmHistoryDebug.HistoryChange(ActiveFlex.History);
 {$ENDIF}
end;

procedure TEditMainForm.acDebugHistoryExecute(Sender: TObject);
begin
 {$IFDEF DEBUG_HISTORY}
 if not Assigned(fmHistoryDebug) then
  fmHistoryDebug := TfmHistoryDebug.Create(Application);
 fmHistoryDebug.Flex := ActiveFlex;
 fmHistoryDebug.Show;
 {$ENDIF}
end;

end.

