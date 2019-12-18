////////////////////////////////////////////
// ����� ��� TCP-������
////////////////////////////////////////////

unit Threads.GMSocket;

interface

uses Windows, Classes, ScktComp, WinSock, GMSocket, SysUtils;

type TGMSocketThread = class(TServerClientThread)
  private
    function GetGMSocket: TGeomerSocket;
  public
    procedure ClientExecute; override;
    constructor Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
    destructor Destroy; override;
    property GMSocket: TGeomerSocket read GetGMSocket;
  end;

implementation

uses ProgramLogFile, ActiveX{$ifdef SQL_APP}, GMSqlQuery{$endif};

constructor TGMSocketThread.Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
begin
  inherited Create(CreateSuspended, ASocket);
end;

destructor TGMSocketThread.Destroy;
begin
  inherited;
{$ifdef SQL_APP}
  DropThreadConnection(ThreadID);
{$endif}
end;

procedure TGMSocketThread.ClientExecute;
var FDSet: TFDSet;
    TimeVal: TTimeVal;
    bTimeSent: bool;
    tStartThread: int64;
    currentAction: string;
    res: Longint;
begin
  bTimeSent := false;
  tStartThread := GetTickCount();
  CoInitialize(nil);

  try
  while not Terminated and ClientSocket.Connected do
  begin
    if (self = nil) or (ClientSocket = nil) or (ClientSocket.SocketHandle = INVALID_SOCKET) then Exit;

    FD_ZERO(FDSet);
    FD_SET(ClientSocket.SocketHandle, FDSet);
    TimeVal.tv_sec := 0;
    TimeVal.tv_usec := 500;

    currentAction := 'select';
    res := select(0, @FDSet, nil, nil, @TimeVal);
    if Terminated then Exit;

    if res > 0 then
    begin
      currentAction := 'ReceiveBuf';
      if ClientSocket.ReceiveBuf(FDSet, -1) = 0 then // ��-�� ����, � �� ��������, �� ���� ���������
      begin
        Break
      end
      else
      begin
        currentAction := 'Event(seRead)';
        Event(seRead);
        bTimeSent := true; // ���� ����� ������ �� ������, �� ����� ���������� �� ����, ��� �� ������ ��� ���������
      end;
    end
    else
    begin
      currentAction := 'GMSocket.CheckCurrentTime';
      // �������� � 5 ��� ����, ����� ������ ���� Ancom ����� ������ �������� ���� ID, �.�. ��� ����� � ���� ����� ������ �������-���
      if not bTimeSent and (Abs(int64(GetTickCount()) - tStartThread) > 5000) then
      begin
        // ��� � ��� �����-�� ������, �������� ��� ����� ������ ��� ��������� ������� ���� �������� ��� �������
        currentAction := 'GMSocket.SendCurrentTime';
        GMSocket.SendCurrentTime();
        bTimeSent := true;
      end;
    end;

    if not Terminated then
    begin
      currentAction := 'GMSocket.BackgroundWork';
      GMSocket.BackgroundWork();
    end;

    if not Terminated then
      Sleep(50);
  end;
  except
    on e: Exception do
    begin
      ProgramLog.AddException('TGMSocketThread.ClientExecute - ' + currentAction + ': ' + e.Message);
    end;
  end;
end;

function TGMSocketThread.GetGMSocket: TGeomerSocket;
begin
  Result := TGeomerSocket(ClientSocket);
end;

end.
