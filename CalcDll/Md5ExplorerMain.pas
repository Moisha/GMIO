unit Md5ExplorerMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  Vcl.Menus, cxMemo, Vcl.StdCtrls, cxButtons, cxLabel, cxTextEdit, cxMaskEdit, cxButtonEdit, cxClasses, dxSkinsForm, dxSkinOffice2010Silver;

type
  TForm8 = class(TForm)
    cxButtonEdit1: TcxButtonEdit;
    cxLabel1: TcxLabel;
    cxButton1: TcxButton;
    cxMemo1: TcxMemo;
    OpenDialog1: TOpenDialog;
    dxSkinController1: TdxSkinController;
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxButton1Click(Sender: TObject);
  private
    procedure PrintMD5(const fn: string);
    procedure PrintVersion(const fn: string);
  public
  end;

var
  Form8: TForm8;

implementation

uses
  IdHashMessageDigest;

{$R *.dfm}

procedure TForm8.PrintVersion(const fn: string);
begin
  var FHandle, FSize: DWORD;

  FSize := GetFileVersionInfoSize(PChar(fn), FHandle);
  if FSize = 0 then
  begin
    Application.MessageBox('Не удалось найти информацию о версии!', PChar(Application.Title), MB_ICONSTOP);
    Exit;
  end;

  var FBuffer: pointer;
  GetMem(FBuffer, FSize);
  try
    if not GetFileVersionInfo(PChar(fn), FHandle, FSize, FBuffer) then
    begin
      Application.MessageBox('Не удалось прочитать информацию о версии!', PChar(Application.Title), MB_ICONSTOP);
      Exit;
    end;

    var Len: UINT;
    var FFI: PVSFixedFileInfo;

    if not VerQueryValue(FBuffer, '\', Pointer(FFI), Len) then
    begin
      Application.MessageBox('Не удалось прочитать информацию о версии!', PChar(Application.Title), MB_ICONSTOP);
      Exit;
    end;

    var s: string;
    s :=
      IntToStr(FFI^.dwProductVersionMS shr 16) + '.' +
      IntToStr(FFI^.dwProductVersionMS and $FFFF) + '.' +
      IntToStr(FFI^.dwProductVersionLS shr 16) + '.' +
      IntToStr(FFI^.dwProductVersionLS and $FFFF);
    cxMemo1.Lines.Add('Версия: ' + s);
//    verSys.DW[0] := FFI^.dwProductVersionLS;
//    verSys.DW[1] := FFI^.dwProductVersionMS;
  finally
    FreeMem(FBuffer);
  end;
end;

procedure TForm8.PrintMD5(const fn: string);
begin
  var f := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  var md5 := TIdHashMessageDigest5.Create;
  try
    cxMemo1.Lines.Add('MD5: ' + md5.HashStreamAsHex(f));
  finally
    md5.Free();
    f.Free();
  end;
end;

procedure TForm8.cxButton1Click(Sender: TObject);
begin
  var fn := OpenDialog1.FileName;
  cxMemo1.Lines.Clear();
  if not FileExists(fn) then
  begin
    Application.MessageBox('Файл не найден!', PChar(Application.Title), MB_ICONSTOP);
    Exit;
  end;

  cxMemo1.Lines.Add('Файл: ' + fn);

  PrintMD5(fn);
  PrintVersion(fn);
end;

procedure TForm8.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if not OpenDialog1.Execute() then
    Exit;

  cxButtonEdit1.Text := OpenDialog1.FileName;
  cxButton1Click(nil);
end;

end.
