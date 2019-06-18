unit SQLNodeReader;

interface

uses SysUtils, GMGlobals, GMSqlQuery, BaseNodeReader, Generics.Collections;

type
  TSQLNodeReader = class (TBaseNodeReader)
  private
    procedure ReaderProc(q: TGMSqlQuery; obj: pointer);
  public
    procedure ReadChildren(ID_Node: int); override;
    procedure AddNew(ID_Parent: int; p: PVTNodeData); override;
    procedure NewCaption(p: PVTNodeData); override;
    procedure Delete(p: PVTNodeData); override;
  end;

implementation

uses Math;

{ TSQLNodeReader }

procedure TSQLNodeReader.ReaderProc(q: TGMSqlQuery; obj: pointer);
var v: TVTNodeData;
begin
  InitVND(@v);

  if obj <> nil then // Nodes
  begin
    v.sTxt := q.FieldByName('Name').AsString;
    v.ObjType := q.FieldByName('ID_NodeType').AsInteger;
    v.ID_Obj := q.FieldByName('ID_Node').AsInteger;
  end
  else
  begin // node channels
    v.sTxt := q.FieldByName('ChnName').AsString;
    v.ID_Obj := q.FieldByName('ID_Node').AsInteger;
    v.ID_Prm := q.FieldByName('ID_Chn').AsInteger;
    v.ID_PT := q.FieldByName('ID_PT').AsInteger;
    v.ID_MeaUnit := q.FieldByName('ID_MeaUnit').AsInteger;
    v.sOpcTag := q.FieldByName('OpcTag').AsString;
    v.BaseChn := q.FieldByName('BaseChn').AsInteger;
  end;

  FReadResults.Add(v);
end;

procedure TSQLNodeReader.AddNew(ID_Parent: int; p: PVTNodeData);
var a: ArrayOfString;
begin
  if p.ObjType > 0 then
  begin
    p.ID_Obj := StrToInt(
                  QueryResult(
                    Format('insert into Nodes(ID_Parent, ID_NodeType, Name) select NullIf(%d, 0), %d, %s returning ID_Node',
                           [ID_Parent, p.ObjType, QuotedStr(p.sTxt)])));
  end
  else
  begin
    a := QueryResultArray_FirstRow(Format('insert into NodeChannels(ID_Node, ID_PT, Name, BaseChn) ' +
                                          '  select %d, %d, %s, %d returning ID_Node, case when Name = '''' then PrmName(BaseChn, 30, ''.'') else Name end',
                                          [ID_Parent, p.ID_PT, QuotedStr(p.sTxt), p.BaseChn]));
    p.ID_Prm := StrToInt(a[0]);
    p.sTxt := a[1];
  end;
end;

procedure TSQLNodeReader.Delete(p: PVTNodeData);
begin
  if p.ID_Prm > 0 then
    ExecSQL(Format('delete from NodeChannels where ID_Chn = %d', [p.ID_Prm]))
  else
    ExecSQL(Format('delete from Nodes where ID_Node = %d', [p.ID_Obj]));
end;

procedure TSQLNodeReader.NewCaption(p: PVTNodeData);
begin
  if p.ID_Prm > 0 then
    ExecSQL(Format('update NodeChannels set Name = %s where ID_Chn = %d', [QuotedStr(p.sTxt), p.ID_Prm]))
  else
    ExecSQL(Format('update Nodes set Name = %s where ID_Node = %d', [QuotedStr(p.sTxt), p.ID_Obj]));
end;

procedure TSQLNodeReader.ReadChildren(ID_Node: int);
begin
  FReadResults.Clear();
  ReadFromQuery('select * from Nodes where coalesce(ID_Parent, 0) = ' + IntToStr(max(ID_Node, 0)), ReaderProc, self);
  if ID_Node > 0 then
    ReadFromQuery('select *, case when Name = '''' then PrmName(BaseChn, 30, ''.'') else Name end as ChnName from Nodechannels where ID_Node = ' + IntToStr(ID_Node), ReaderProc);
end;

end.
