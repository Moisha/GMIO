unit fLibProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, RxToolEdit, ExtCtrls, FlexLibs,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus, dxSkinOffice2010Silver;

type
  TfmLibProps = class(TForm)
    bbOk: TcxButton;
    bbCancel: TcxButton;
    Panel1: TcxGroupBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    edTitle: TcxTextEdit;
    Label3: TcxLabel;
    mmDesc: TcxMemo;
    fedLibFilename: TcxButtonEdit;
    OpenDialog1: TOpenDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure fedLibFilenamePropertiesChange(Sender: TObject);
  private
    { Private declarations }
    FFlexLibrary: TFlexLibrary;
    procedure SetFlexLibrary(const Value: TFlexLibrary);
  public
    { Public declarations }
    property  FlexLibrary: TFlexLibrary read FFlexLibrary write SetFlexLibrary;
  end;

var
  fmLibProps: TfmLibProps;

implementation

{$R *.DFM}

procedure TfmLibProps.SetFlexLibrary(const Value: TFlexLibrary);
begin
 if Value = FFlexLibrary then exit;
 FFlexLibrary := Value;
 if Assigned(FFlexLibrary) then begin
  fedLibFilename.Text := FFlexLibrary.LibFilename;
  edTitle.Text := FFlexLibrary.Name;
  mmDesc.Lines.Text := FFlexLibrary.Hint;
 end;
end;

procedure TfmLibProps.fedLibFilenamePropertiesChange(Sender: TObject);
begin
 if OpenDialog1.Execute() then
 begin
  fedLibFilename.Text := ExtractRelativePath( ExtractFilePath(ParamStr(0)), OpenDialog1.FileName );
 end;
end;

procedure TfmLibProps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if ModalResult = mrOk then begin
  FFlexLibrary.LibFilename := fedLibFilename.Text;
  FFlexLibrary.Name := edTitle.Text;
  FFlexLibrary.Hint := mmDesc.Lines.Text;
  FFlexLibrary.Modified := true;
 end;
end;

end.
