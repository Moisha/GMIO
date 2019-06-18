/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    FlexGraphics library file formats support        //
//    Standard raster files                            //
//                                                     //
/////////////////////////////////////////////////////////

unit FormatStdFiles;

{$I FlexDefs.inc}

interface

uses
  Windows, Types, UITypes, Graphics, SysUtils, Classes, FlexBase, FlexFileFormats;

type
  TFlexStandardFormats = class(TFlexFileFormat)
  protected
    procedure RegisterSupportedExtensions; override;
  public
    procedure ImportFromStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); override;
    procedure ExportToStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); override;
  end;

implementation

uses
  Consts, Jpeg, JConsts, FlexControls, FlexUtils;

{$IFNDEF FG_D5}
resourcestring
  sJPEGImageFile = 'JPEG Image File';
{$ENDIF}

type
  TStdFormatExt = (
    sfxNone,
    sfxBitmap,
    sfxIcon,
    sfxJpeg
  );

// TFlexStandardFormats ///////////////////////////////////////////////////////

procedure TFlexStandardFormats.RegisterSupportedExtensions;
begin
  // Register standard extensions
  RegisterExtension('ico', SVIcons, [skImport, skExport], integer(sfxIcon));
  RegisterExtension('bmp', SVBitmaps, [skImport, skExport], integer(sfxBitmap));
  RegisterExtension('jpeg', sJPEGImageFile, [skImport, skExport],
    integer(sfxJpeg));
  RegisterExtension('jpg', sJPEGImageFile, [skImport, skExport],
    integer(sfxJpeg));
end;

procedure TFlexStandardFormats.ExportToStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
var
  Bitmap: TBitmap;
  Graphic: TGraphic;
  IconHandle: cardinal;
  IconInfo: TIconInfo;
begin
  Graphic := Nil;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ScaleValue(AFlexPanel.DocWidth, 100);
    Bitmap.Height := ScaleValue(AFlexPanel.DocHeight, 100);
    AFlexPanel.PaintTo(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height),
      Point(0, 0), 100, AFlexPanel.ActiveScheme, False, False, False, True);
    case TStdFormatExt(Extension.Tag) of
      sfxBitmap:
        begin
          Graphic := Bitmap;
          Bitmap := Nil;
        end;
      sfxIcon:
        begin
          FillChar(IconInfo, SizeOf(IconInfo), 0);
          with IconInfo do begin
            fIcon := True;
            hbmMask := Bitmap.Handle;
            hbmColor := Bitmap.Handle;
          end;
          IconHandle := CreateIconIndirect(IconInfo);
          if IconHandle = 0 then
            raise Exception.CreateFmt(sExportError,
              [SysErrorMessage(GetLastError)]);
          Graphic := TIcon.Create;
          TIcon(Graphic).Handle := IconHandle;
        end;
      sfxJpeg:
        begin
          Graphic := TJpegImage.Create;
          Graphic.Assign(Bitmap);
        end;
      else
        raise Exception.CreateFmt(sUnsupportedFormat, [Extension.Extension]);
    end;
    Graphic.SaveToStream(AStream);
  finally
    Bitmap.Free;
    Graphic.Free;
  end;
end;

procedure TFlexStandardFormats.ImportFromStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
var
  Graphic: TGraphic;
  FlexPicture: TFlexPicture;
begin
  Graphic := Nil;
  try
    case TStdFormatExt(Extension.Tag) of
      sfxBitmap:
        Graphic := TBitmap.Create;
      sfxIcon:
        Graphic := TIcon.Create;
      sfxJpeg:
        Graphic := TJpegImage.Create;
      else
        raise Exception.CreateFmt(sUnsupportedFormat, [Extension.Extension]);
    end;
    Graphic.LoadFromStream(AStream);
    FlexPicture := TFlexPicture.Create(AFlexPanel, AFlexPanel.ActiveScheme,
      AFlexPanel.ActiveLayer);
    FlexPicture.PictureProp.Graphic := Graphic;
    FlexPicture.AutoSizeProp.Value := True;
    FlexPicture.AutoSizeProp.Value := False;
    AFlexPanel.DocWidth := FlexPicture.WidthProp.Value;
    AFlexPanel.DocHeight := FlexPicture.HeightProp.Value;
  finally
    Graphic.Free;
  end;
end;

initialization
  RegisteredFlexFileFormats.RegisterFormat(TFlexStandardFormats);

end.
