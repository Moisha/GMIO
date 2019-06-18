unit fDocProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FlexBase, FlexProps, FlexUtils, RXSpin, Vcl.Mask,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TfmDocProps = class(TForm)
    panProps: TcxGroupBox;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    Label1: TcxLabel;
    edTitle: TcxTextEdit;
    Label2: TcxLabel;
    mmComment: TcxMemo;
    Label3: TcxLabel;
    Label4: TcxLabel;
    bbUserData: TcxButton;
    Label5: TcxLabel;
    edVersion: TcxTextEdit;
    Label12: TcxLabel;
    sedWidth: TcxSpinEdit;
    sedHeight: TcxSpinEdit;
    sedConnectorsMinGap: TcxSpinEdit;
    chSaveAsBinary: TcxCheckBox;
    chKeepLink: TcxCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbUserDataClick(Sender: TObject);
  private
    { Private declarations }
    FFlex: TFlexPanel;
    UserProps: TPropList;
    UserData: TUserDataProp;
  public
    { Public declarations }
  end;

var
  fmDocProps: TfmDocProps;

implementation

{$R *.DFM}

procedure TfmDocProps.FormCreate(Sender: TObject);
begin
 UserProps := TPropList.Create(Nil);
 UserData := TUserDataProp.Create(UserProps, 'User data');
end;

procedure TfmDocProps.FormDestroy(Sender: TObject);
begin
 UserProps.Free;
end;

procedure TfmDocProps.FormShow(Sender: TObject);
var Ver: integer;
begin
 if (Tag <> 0) and (TObject(Tag) is TFlexPanel) then
  FFlex := TFlexPanel(Tag);
 if Assigned(FFlex) then begin
  edTitle.Text := FFlex.Schemes.Name;
  mmComment.Lines.Text := FFlex.Schemes.Hint;
  sedWidth.Value := FFlex.DocWidth / PixelScaleFactor;
  sedHeight.Value := FFlex.DocHeight / PixelScaleFactor;
  UserData.Text := FFlex.Schemes.UserData.Text;
  Ver := FFlex.Schemes.VersionProp.Value;
  edVersion.Text := Format('%d.%d', [Ver div 100, Ver mod 100]);
  sedConnectorsMinGap.Value := FFlex.ConnectorsMinGap / PixelScaleFactor;
  chSaveAsBinary.Checked := FFlex.SaveAsBinary;
  chKeepLink.Checked := FFlex.Schemes.ConnectorsKeepLinkProp.Value;
 end else begin
  panProps.Enabled := false;
  bbUserData.Enabled := false;
  bbOk.Enabled := false;
 end;
end;

procedure TfmDocProps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (ModalResult = mrOk) and Assigned(FFlex) then begin
  FFlex.Schemes.Name := edTitle.Text;
  FFlex.Schemes.Hint := mmComment.Lines.Text;
  FFlex.Schemes.UserData.Text := UserData.Text;
  FFlex.DocWidth := Round(sedWidth.Value * PixelScaleFactor);
  FFlex.DocHeight := Round(sedHeight.Value * PixelScaleFactor);
  FFlex.ConnectorsMinGap := Round(sedConnectorsMinGap.Value * PixelScaleFactor);
  FFlex.SaveAsBinary := chSaveAsBinary.Checked;
  FFlex.Schemes.ConnectorsKeepLinkProp.Value := chKeepLink.Checked;
  ModalResult := mrOk;
 end;
 Action := caHide;
end;

procedure TfmDocProps.bbUserDataClick(Sender: TObject);
begin
 UserData.Edit;
end;

end.
