unit BaseNodeReader;

interface
uses GMGlobals, Generics.Collections;

type
  TBaseNodeReader = class
    FReadResults: TList <TVTNodeData>;
  public
    constructor Create;
    destructor Destroy; override;
    property ReadResult: TList <TVTNodeData> read FReadResults;

    procedure ReadChildren(ID_Node: int); virtual; abstract;
    procedure AddNew(ID_Parent: int; p: PVTNodeData); virtual; abstract;
    procedure NewCaption(p: PVTNodeData); virtual; abstract;
    procedure Delete(p: PVTNodeData); virtual; abstract;
  end;

  TBaseNodeReaderClass = class of TBaseNodeReader;

implementation

{ TBaseNodeReader }

constructor TBaseNodeReader.Create;
begin
  inherited;

  FReadResults := TList<TVTNodeData>.Create();
end;

destructor TBaseNodeReader.Destroy;
begin
  FReadResults.Free();
  inherited;
end;

end.
