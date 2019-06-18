unit Frames.ScriptEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvEditorCommon, JvEditor, JvHLEditor, dxSkinsCore,
  dxSkinOffice2010Silver, dxSkinsdxBarPainter, dxBar, cxClasses, Vcl.Menus;

type
  TScriptEditorFrame = class(TFrame)
    mScript: TJvHLEditor;
    dxBarManager1: TdxBarManager;
    dxBarPopupMenu1: TdxBarPopupMenu;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
