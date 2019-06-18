unit fOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, RxCombos, RXSpin, ColorComboEdit, 
  FlexBase, FlexUtils, Vcl.Mask,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TOptionEditPage = ( opDocument, opGrid, opDuplicates, opConnectors );
  TOptionEditPages = set of TOptionEditPage;

const
  AllOptions = [opDocument, opGrid];

type
  PEditOptions = ^TEditOptions;
  TEditOptions = record
   // Document props
   DocWidth: integer;
   DocHeight: integer;
   DocAutoNameNumbs: boolean;
   // Grid props
   ShowGrid: boolean;
   SnapToGrid: boolean;
   SnapStyle: TFlexSnaps;
   ShowPixGrid: boolean;
   GridStyle: TFlexGridStyle;
   GridColor: TColor;
   GridPixColor: TColor;
   GridHSize: integer;
   GridVSize: integer;
   GridHOffset: integer;
   GridVOffset: integer;
   // Duplicates
   ShiftX: integer;
   ShiftY: integer;
   DupRandom: boolean;
  end;

  TfmOptions = class(TForm)
    bbOk: TcxButton;
    bbClose: TcxButton;
    pgOptions: TcxPageControl;
    tsDocProps: TcxTabSheet;
    gbDocSize: TcxGroupBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    chAutoNameNumbs: TcxCheckBox;
    tsGridProps: TcxTabSheet;
    gbGridSize: TcxGroupBox;
    chShowGrid: TcxCheckBox;
    chSnapToGrid: TcxCheckBox;
    chShowPixGrid: TcxCheckBox;
    panGridStyle: TcxGroupBox;
    rbGridAsLines: TcxRadioButton;
    rbGridAsDots: TcxRadioButton;
    Label5: TcxLabel;
    Label6: TcxLabel;
    Label7: TcxLabel;
    gbGridOffset: TcxGroupBox;
    Label12: TcxLabel;
    Label13: TcxLabel;
    gbSnapStyle: TcxGroupBox;
    chSnapLeft: TcxCheckBox;
    chSnapCenter: TcxCheckBox;
    chSnapTop: TcxCheckBox;
    chSnapBottom: TcxCheckBox;
    chSnapRight: TcxCheckBox;
    tsDupProps: TcxTabSheet;
    gbDupShift: TcxGroupBox;
    Label8: TcxLabel;
    Label9: TcxLabel;
    Label10: TcxLabel;
    Label11: TcxLabel;
    chDupRandom: TcxCheckBox;
    sedWidth: TcxSpinEdit;
    sedHeight: TcxSpinEdit;
    sedGridHSize: TcxSpinEdit;
    sedGridVSize: TcxSpinEdit;
    sedGridHOffset: TcxSpinEdit;
    sedGridVOffset: TcxSpinEdit;
    sedDupShiftX: TcxSpinEdit;
    sedDupShiftY: TcxSpinEdit;
    cceGridColor: TcxColorComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ctrlDocChange(Sender: TObject);
    procedure ctrlGridClick(Sender: TObject);
    procedure ctrlGridChange(Sender: TObject);
    procedure ctrlDupChange(Sender: TObject);
    procedure ctrlDupClick(Sender: TObject);
    procedure ctrlConnectorsChange(Sender: TObject);
  private
    { Private declarations }
    FOptions: TEditOptions;
    FEdited: TOptionEditPages;
    procedure ReadFromOptions;
    procedure WriteToOptions;
    procedure SetOptions(const Value: TEditOptions);
    function GetPage: TOptionEditPage;
    procedure SetPage(const Value: TOptionEditPage);
  public
    { Public declarations }
    property  Edited: TOptionEditPages read FEdited;
    property  EditOptions: TEditOptions read FOptions write SetOptions;
    property  Page: TOptionEditPage read GetPage write SetPage;
  end;

var
  fmOptions: TfmOptions;
  EditOptions: TEditOptions;

implementation

{$R *.DFM}

uses
  FlexControls;

procedure InitDefaultOptions;
begin
 with EditOptions do begin
  // Document props
  DocWidth := 640 * PixelScaleFactor;
  DocHeight := 480 * PixelScaleFactor;
  DocAutoNameNumbs := true;
  // Grid props
  ShowGrid := False;
  SnapToGrid := False;
  SnapStyle := [snLeft, snTop];
  ShowPixGrid := False;
  GridStyle := gsDots;
  GridColor := clGray;
  GridPixColor := clSilver;
  GridHSize := 10 * PixelScaleFactor;
  GridVSize := 10 * PixelScaleFactor;
  GridHOffset := 0;
  GridVOffset := 0;
  // Duplicates
  ShiftX := 10 * PixelScaleFactor;
  ShiftY := 10 * PixelScaleFactor;
  DupRandom := False;
 end;
end;

// TfmOptions /////////////////////////////////////////////////////////////////

procedure TfmOptions.FormCreate(Sender: TObject);
begin
 pgOptions.ActivePage := tsDocProps;
end;

procedure TfmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 WriteToOptions;
 Action := caHide;
end;

function TfmOptions.GetPage: TOptionEditPage;
begin
 if pgOptions.ActivePage = tsDocProps then Result := opDocument else
 if pgOptions.ActivePage = tsGridProps then Result := opGrid else
 if pgOptions.ActivePage = tsDupProps then Result := opDuplicates else
 //if pgOptions.ActivePage = tsConnectors then Result := opConnectors else
  Result := opDocument;
end;

procedure TfmOptions.SetPage(const Value: TOptionEditPage);
begin
 case Value of
  opDocument   : pgOptions.ActivePage := tsDocProps;
  opGrid       : pgOptions.ActivePage := tsGridProps;
  opDuplicates : pgOptions.ActivePage := tsDupProps;
  //opConnectors : pgOptions.ActivePage := tsConnectors;
 end;
end;

procedure TfmOptions.SetOptions(const Value: TEditOptions);
begin
 FOptions := Value;
 ReadFromOptions;
end;

procedure TfmOptions.ReadFromOptions;
var SnapAll: boolean;
begin
 with FOptions do begin
  sedWidth.Value := DocWidth / PixelScaleFactor;
  sedHeight.Value := DocHeight / PixelScaleFactor;
  chAutoNameNumbs.Checked := DocAutoNameNumbs;
  chShowGrid.Checked := ShowGrid;
  chSnapToGrid.Checked := SnapToGrid;
  SnapAll := snAll in SnapStyle;
  chSnapLeft.Checked := SnapAll or (snLeft in SnapStyle);
  chSnapRight.Checked := SnapAll or (snRight in SnapStyle);
  chSnapTop.Checked := SnapAll or (snTop in SnapStyle);
  chSnapBottom.Checked := SnapAll or (snBottom in SnapStyle);
  chSnapCenter.Checked := SnapAll or (snCenter in SnapStyle);
  chShowPixGrid.Checked := ShowPixGrid;
  case GridStyle of
   gsLines : rbGridAsLines.Checked := true;
   gsDots  : rbGridAsDots.Checked := true;
  end;
  sedGridHSize.Value := GridHSize / PixelScaleFactor;
  sedGridVSize.Value := GridVSize / PixelScaleFactor;
  sedGridHOffset.Value := GridHOffset / PixelScaleFactor;
  sedGridVOffset.Value := GridVOffset / PixelScaleFactor;
  cceGridColor.ColorValue := GridColor;
  sedDupShiftX.Value := ShiftX / PixelScaleFactor;
  sedDupShiftY.Value := ShiftY / PixelScaleFactor;
  chDupRandom.Checked := DupRandom;
  // sedConnectorsMinGap.Value := ConnectorsMinGap / PixelScaleFactor;
 end;
 FEdited := [];
end;

procedure TfmOptions.WriteToOptions;
begin
 // Documents
 FOptions.DocWidth := Round(sedWidth.Value * PixelScaleFactor);
 FOptions.DocHeight := Round(sedHeight.Value * PixelScaleFactor);
 FOptions.DocAutoNameNumbs := chAutoNameNumbs.Checked;
 // Grid
 FOptions.ShowGrid := chShowGrid.Checked;
 FOptions.SnapToGrid := chSnapToGrid.Checked;
 FOptions.SnapStyle := [];
 if chSnapLeft.Checked then Include(FOptions.SnapStyle, snLeft);
 if chSnapRight.Checked then Include(FOptions.SnapStyle, snRight);
 if chSnapTop.Checked then Include(FOptions.SnapStyle, snTop);
 if chSnapBottom.Checked then Include(FOptions.SnapStyle, snBottom);
 if chSnapCenter.Checked then Include(FOptions.SnapStyle, snCenter);
 FOptions.ShowPixGrid := chShowPixGrid.Checked;
 if rbGridAsLines.Checked then
  FOptions.GridStyle := gsLines
 else
 if rbGridAsDots.Checked then
  FOptions.GridStyle := gsDots;
 FOptions.GridHSize := Round(sedGridHSize.Value * PixelScaleFactor);
 FOptions.GridVSize := Round(sedGridVSize.Value * PixelScaleFactor);
 FOptions.GridHOffset := Round(sedGridHOffset.Value * PixelScaleFactor);
 FOptions.GridVOffset := Round(sedGridVOffset.Value * PixelScaleFactor);
 FOptions.GridColor := cceGridColor.ColorValue;
 // Duplicates
 FOptions.ShiftX := Round(sedDupShiftX.Value * PixelScaleFactor);
 FOptions.ShiftY := Round(sedDupShiftY.Value * PixelScaleFactor);
 FOptions.DupRandom := chDupRandom.Checked;
 // Connectors
 // FOptions.ConnectorsMinGap := Round(sedConnectorsMinGap.Value * PixelScaleFactor);
end;

procedure TfmOptions.ctrlDocChange(Sender: TObject);
begin
 Include(FEdited, opDocument);
end;

procedure TfmOptions.ctrlGridClick(Sender: TObject);
begin
 Include(FEdited, opGrid);
end;

procedure TfmOptions.ctrlGridChange(Sender: TObject);
begin
 Include(FEdited, opGrid);
end;

procedure TfmOptions.ctrlDupChange(Sender: TObject);
begin
 Include(FEdited, opDuplicates);
end;

procedure TfmOptions.ctrlDupClick(Sender: TObject);
begin
 Include(FEdited, opDuplicates);
end;

procedure TfmOptions.ctrlConnectorsChange(Sender: TObject);
begin
 Include(FEdited, opConnectors);
end;

initialization
  InitDefaultOptions;

end.
