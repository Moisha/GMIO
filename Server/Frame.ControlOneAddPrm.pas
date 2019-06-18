////////////////////////////////////////////
// Вспомогательные компоненты для настройки управления
////////////////////////////////////////////
unit Frame.ControlOneAddPrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls, GMGlobals;

type
  TControlOneAddPrmFrame = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    eCaption: TEdit;
    lePrm: TLabeledEdit;
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ControlPrmType: int;
  end;

implementation

{$R *.dfm}

procedure TControlOneAddPrmFrame.Panel1Resize(Sender: TObject);
begin
  lePrm.Width := Panel1.Width - lePrm.Left - 4;
  eCaption.Width := Panel1.Width - eCaption.Left - 4; 
end;

end.
