/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    FlexGraphics library file formats support        //
//    Base classes                                     //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexFileFormats;

{$I FlexDefs.inc}

interface

uses
  Classes, SysUtils, FlexBase;

resourcestring
  sUnsupportedFormat = 'Unsupported format %s';
  sImportError       = 'Import error: %s';
  sExportError       = 'Export error: %s';
  sNotImplemented    = 'Not implemented';

type
  TFlexFileFormat = class;
  TFlexFileFormatClass = class of TFlexFileFormat;
  TFlexFileExtensions = class;

  TFlexFileSupportKind = (
    skImport,
    skExport
  );

  TFlexFileSupportKinds = set of TFlexFileSupportKind;

  TFlexFileExtension = class
  public
    FormatClass: TFlexFileFormatClass;
    Extension: string;
    Description: string;
    Kinds: TFlexFileSupportKinds;
    Tag: integer;
  end;

  TFlexFileFormat = class
  protected
    // FOwner initialized only when constructed via Create()
    FOwner: TObject;
    // Determines is the class supports streams. If False then class can
    // operate with AFilename parameter only (caller should set AStream to Nil).
    // True by default.
    FStreamSupport: boolean;
    // FExtensions initialized only when constructed via CreateAndRegister()
    FExtensions: TFlexFileExtensions;
    function RegisterExtension(const AExtension, ADescription: string;
      AKinds: TFlexFileSupportKinds; ATag: integer = 0): integer; virtual;
    procedure RegisterSupportedExtensions; virtual; abstract;
  public
    constructor Create(AOwner: TObject); virtual;
    constructor CreateForRegister(AExtensions: TFlexFileExtensions);
    destructor Destroy; override;
    procedure ImportFromStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); virtual;
    procedure ExportToStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); virtual;
    property Owner: TObject read FOwner;
    property StreamSupport: boolean read FStreamSupport;
  end;

  TFlexFileExtensions = class
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TFlexFileExtension;
  protected
    function RegisterExtension(Extension: TFlexFileExtension): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function  RegisterFormat(AFormat: TFlexFileFormatClass): boolean;
    procedure UnregisterFormat(AFormat: TFlexFileFormatClass);
    procedure Delete(Index: integer);
    procedure Clear;
    function IndexOf(AClass: TFlexFileFormatClass): integer;
    procedure ImportFromStream(ItemIndex: integer; AFlexPanel: TFlexPanel;
      AStream: TStream; AFileName: string = '');
    procedure ExportToStream(ItemIndex: integer; AFlexPanel: TFlexPanel;
      AStream: TStream; AFileName: string = '');
    procedure ImportFromFile(ItemIndex: integer; AFlexPanel: TFlexPanel;
      AFilename: string);
    procedure ExportToFile(ItemIndex: integer; AFlexPanel: TFlexPanel;
      AFilename: string);
    property Count: integer read GetCount;
    property Items[Index: integer]: TFlexFileExtension read GetItem; default;
  end;

function RegisteredFlexFileFormats: TFlexFileExtensions;

implementation

{$IFDEF STDFILEFORMATS}
uses
  FormatMetaFile, FormatStdFiles, FormatSvgFile;
{$ENDIF}

var
  FFormats: TFlexFileExtensions;

function RegisteredFlexFileFormats: TFlexFileExtensions;
begin
  if not Assigned(FFormats) then
    FFormats := TFlexFileExtensions.Create;
  Result := FFormats;
end;

// TFlexFileFormat ////////////////////////////////////////////////////////////

constructor TFlexFileFormat.Create(AOwner: TObject);
begin
  FOwner := AOwner;
  FStreamSupport := True;
end;

constructor TFlexFileFormat.CreateForRegister(AExtensions: TFlexFileExtensions);
begin
  FExtensions := AExtensions;
  RegisterSupportedExtensions;
end;

destructor TFlexFileFormat.Destroy;
begin
  inherited;
end;

function TFlexFileFormat.RegisterExtension(const AExtension,
  ADescription: string; AKinds: TFlexFileSupportKinds;
  ATag: integer = 0): integer;
var
  Item: TFlexFileExtension;
begin
  Item := TFlexFileExtension.Create;
  with Item do
  try
    FormatClass := TFlexFileFormatClass(Self.ClassType);
    Extension := AExtension;
    Description := ADescription;
    Kinds := AKinds;
    Tag := ATag;
    Result := FExtensions.RegisterExtension(Item);
  except
    Free;
    raise;
  end;
end;

procedure TFlexFileFormat.ImportFromStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
begin
  raise Exception.CreateFmt(sImportError, [sNotImplemented]);
end;

procedure TFlexFileFormat.ExportToStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
begin
  raise Exception.CreateFmt(sExportError, [sNotImplemented]);
end;

// TRegisteredFlexFileFormats /////////////////////////////////////////////////

constructor TFlexFileExtensions.Create;
begin
  FItems := TList.Create;
end;

destructor TFlexFileExtensions.Destroy;
begin
  Clear;
  inherited;
  FItems.Free;
end;

function TFlexFileExtensions.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TFlexFileExtensions.GetItem(Index: integer): TFlexFileExtension;
begin
  Result := TFlexFileExtension(FItems[Index]);
end;

function TFlexFileExtensions.IndexOf(AClass: TFlexFileFormatClass): integer;
var
  i: integer;
begin
  Result := -1;
  for i:=0 to FItems.Count-1 do
    if TFlexFileExtension(FItems[i]).FormatClass = AClass then begin
      Result := i;
      Break;
    end;
end;

procedure TFlexFileExtensions.Delete(Index: integer);
var
  Item: TFlexFileExtension;
begin
  Item := FItems[Index];
  FItems.Delete(Index);
  Item.Free;
end;

procedure TFlexFileExtensions.Clear;
var
  i: integer;
begin
  for i:=0 to FItems.Count-1 do
    TFlexFileExtension(FItems[i]).Free;
  FItems.Clear;
end;

function TFlexFileExtensions.RegisterFormat(
  AFormat: TFlexFileFormatClass): boolean;
var
  i: integer;
begin
  Result := True;
  // Check if the AFormat class already have registered extensions
  for i:=0 to FItems.Count-1 do
    if TFlexFileExtension(FItems[i]).FormatClass = AFormat then begin
      Result := False;
      Break;
    end;
  // Register all format extensions
  if Result then
    AFormat.CreateForRegister(Self).Free;
end;

procedure TFlexFileExtensions.UnregisterFormat(AFormat: TFlexFileFormatClass);
var
  i: integer;
begin
  for i:=FItems.Count-1 downto 0 do
    if TFlexFileExtension(FItems[i]).FormatClass = AFormat then
      Delete(i);
end;

function TFlexFileExtensions.RegisterExtension(
  Extension: TFlexFileExtension): integer;
begin
  Result := FItems.IndexOf(Extension);
  if Result < 0 then
    Result := FItems.Add(Extension);
end;

procedure TFlexFileExtensions.ImportFromStream(ItemIndex: integer;
  AFlexPanel: TFlexPanel; AStream: TStream; AFileName: string = '');
begin
  with TFlexFileExtension(FItems[ItemIndex]).FormatClass.Create(Self) do
    try
      ImportFromStream(AStream, AFlexPanel,
        TFlexFileExtension(FItems[ItemIndex]), AFileName);
    finally
      Free;
    end;
end;

procedure TFlexFileExtensions.ExportToStream(ItemIndex: integer;
  AFlexPanel: TFlexPanel; AStream: TStream; AFileName: string = '');
begin
  with TFlexFileExtension(FItems[ItemIndex]).FormatClass.Create(Self) do
    try
      ExportToStream(AStream, AFlexPanel,
        TFlexFileExtension(FItems[ItemIndex]), AFileName);
    finally
      Free;
    end;
end;

procedure TFlexFileExtensions.ImportFromFile(ItemIndex: integer;
  AFlexPanel: TFlexPanel; AFilename: string);
var
  FS: TFileStream;
begin
  FS := Nil;
  with TFlexFileExtension(FItems[ItemIndex]).FormatClass.Create(Self) do
    try
      if StreamSupport then FS := TFileStream.Create(AFilename, fmOpenRead);
      ImportFromStream(FS, AFlexPanel,
        TFlexFileExtension(FItems[ItemIndex]), AFileName);
    finally
      FS.Free;
      Free;
    end;
end;

procedure TFlexFileExtensions.ExportToFile(ItemIndex: integer;
  AFlexPanel: TFlexPanel; AFilename: string);
var
  FS: TFileStream;
begin
  FS := Nil;
  with TFlexFileExtension(FItems[ItemIndex]).FormatClass.Create(Self) do
    try
      if StreamSupport then FS := TFileStream.Create(AFilename, fmCreate);
      ExportToStream(FS, AFlexPanel,
        TFlexFileExtension(FItems[ItemIndex]), AFileName);
    finally
      FS.Free;
      Free;
    end;
end;

///////////////////////////////////////////////////////////////////////////////

initialization

finalization
  FFormats.Free;

end.
