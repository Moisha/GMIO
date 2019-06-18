unit ArrayNodeReader;

interface

uses BaseNodeReader, GMGlobals, SysUtils;

type
  TArrayNodeReader = class(TBaseNodeReader)
  public
    procedure ReadChildren(ID_Node: int); override;
    procedure AddNew(ID_Parent: int; p: PVTNodeData); override;
    procedure NewCaption(p: PVTNodeData); override;
    procedure Delete(p: PVTNodeData); override;
  end;

implementation

{ TArrayNodeReader }

uses GMDBClasses;

procedure TArrayNodeReader.AddNew(ID_Parent: int; p: PVTNodeData);
begin
  raise Exception.Create('Edit not allowed!');
end;

procedure TArrayNodeReader.Delete(p: PVTNodeData);
begin
  raise Exception.Create('Edit not allowed!');
end;

procedure TArrayNodeReader.NewCaption(p: PVTNodeData);
begin
  raise Exception.Create('Edit not allowed!');
end;

procedure TArrayNodeReader.ReadChildren(ID_Node: int);
var nd: TGMNode;
    chn: TGMNodeChannel;
    rec: TVTNodeData;
begin
  ReadResult.Clear();
  for nd in GMNodes do
  begin
    if nd.ID_Parent <> ID_Node then continue;

    InitVND(@rec);
    rec.sTxt := nd.Name;
    rec.ID_Obj := nd.ID_Node;
    rec.ObjType := nd.NodeType;
    rec.Obj := nd;

    ReadResult.Add(rec);
  end;

  for chn in GMNodeChannels do
  begin
    if (chn.Node = nil) or (chn.Node.ID_Node <> ID_Node) then continue;

    InitVND(@rec);
    rec.ID_Prm := chn.ID_Prm;
    rec.sTxt := chn.Caption;
    rec.Obj := chn;

    ReadResult.Add(rec);
  end;
end;

end.
