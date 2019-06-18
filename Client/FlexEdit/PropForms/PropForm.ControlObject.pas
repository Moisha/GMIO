unit PropForm.ControlObject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Frame.ObjectTree, Vcl.StdCtrls, Vcl.ExtCtrls, FlexProps, GMConst, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore, dxSkinOffice2010Silver,
  Vcl.Menus, cxButtons, cxGroupBox;

type
  TControlObjectPropForm = class(TForm)
    Panel1: TcxGroupBox;
    Button1: TcxButton;
    Button2: TcxButton;
    frmObjTree: TObjTreeFrame;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ControlObjectPropForm: TControlObjectPropForm;

implementation

{$R *.dfm}

uses GMDBClasses;

procedure TControlObjectPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult <> mrOk) or (Tag <= 0) then Exit;

  if frmObjTree.SelectedObject <> nil then
    TControlObjectProp(Tag).Value := TGMObject(frmObjTree.SelectedObject).ID_Obj;
end;

procedure TControlObjectPropForm.FormShow(Sender: TObject);
begin
  frmObjTree.Init();
  frmObjTree.HideNodeTree();
  frmObjTree.FindObject(GMObjects.ObjByID(TControlObjectProp(Tag).Value));
end;

initialization
  RegisterDefaultPropEditForm(TControlObjectProp, TControlObjectPropForm);
end.
