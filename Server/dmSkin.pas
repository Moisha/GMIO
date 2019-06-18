unit dmSkin;

interface

uses
  System.SysUtils, System.Classes, dxLayoutLookAndFeels, cxClasses, dxSkinsCore, dxSkinOffice2010Silver, cxLookAndFeels, dxSkinsForm;

type
  TDataModuleSkin = class(TDataModule)
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel;
    dxSkinController1: TdxSkinController;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleSkin: TDataModuleSkin;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
