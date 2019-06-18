unit PropTxtEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TPropertyTextEditor = class(TForm)
    Panel1: TPanel;
    mEditor: TMemo;
    btnCancel: TButton;
    btnOk: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CheckControlsPosition;
    { Private declarations }
  public
    { Public declarations }
    function ShowModal(const ACaption, AValue: string): TModalResult; reintroduce;
  end;

implementation

{$R *.dfm}

procedure TPropertyTextEditor.CheckControlsPosition();
begin
  Panel1.Width := self.ClientWidth;
  btnCancel.Left := btnCancel.Parent.Width - btnCancel.Width - 8;
  btnOk.Left := btnCancel.Left - btnOk.Width - 8;
end;

procedure TPropertyTextEditor.FormResize(Sender: TObject);
begin
  CheckControlsPosition();
end;

procedure TPropertyTextEditor.FormCreate(Sender: TObject);
begin
  CheckControlsPosition();
end;

function TPropertyTextEditor.ShowModal(const ACaption, AValue: string): TModalResult;
begin
  Caption := ACaption;
  mEditor.Lines.Text := AValue;

  Result := inherited ShowModal();
end;

end.
