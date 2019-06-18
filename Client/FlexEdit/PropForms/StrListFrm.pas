unit StrListFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FlexProps, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, dxSkinOffice2010Silver, Vcl.Menus, cxButtons, cxTextEdit, cxMemo;

type
  TStrListPropForm = class(TForm)
    mmText: TcxMemo;
    BitBtn1: TcxButton;
    BitBtn2: TcxButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FStrListProp: TStrListProp;
  public
    { Public declarations }
  end;

var
  StrListPropForm: TStrListPropForm;

implementation

{$R *.DFM}

procedure TStrListPropForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TStrListProp) then
  FStrListProp := TStrListProp(Tag);
 if Assigned(FStrListProp) then
  mmText.Text := FStrListProp.Text;
end;

procedure TStrListPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (ModalResult <> mrOk) or not Assigned(FStrListProp) then exit;
 FStrListProp.Text := mmText.Text;
end;

initialization
  RegisterDefaultPropEditForm(TStrListProp, TStrListPropForm);

end.
