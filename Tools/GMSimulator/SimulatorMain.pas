unit SimulatorMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GMGlobals, GMConst, Vcl.Buttons, Threads.Base,
  Connection.TCP, Connection.Base;

type
  TEmulationThread = class(TGMThread)
  private
    con: TConnectionObjectTCP_OwnSocket;
    function PrepareBuffer(n_car, utime: int): ArrayOfByte;
  protected
    procedure SafeExecute; override;
    constructor Create(const ip: string; port: int);
  end;

  TEmulationThreadPack = class(TEmulationThread)
  private
    FCount, FNCar: int;
  protected
    procedure SafeExecute; override;
    constructor Create(const ip: string; port, ncar, count: int);
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    btnGo: TSpeedButton;
    eNCar: TEdit;
    Label1: TLabel;
    eCount: TEdit;
    Label2: TLabel;
    Button1: TButton;
    ePort: TEdit;
    Label3: TLabel;
    procedure btnGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    thrEmulate: TEmulationThread;
    thrEmulatePack: TEmulationThreadPack;
    procedure EmulationThreadPackDone(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnGoClick(Sender: TObject);
begin
  btnGo.Down := not btnGo.Down;
  if btnGo.Down then
    thrEmulate := TEmulationThread.Create('127.0.0.1', StrToInt(ePort.Text))
  else
    thrEmulate.Free();
end;

{ TEmulationThread }

constructor TEmulationThread.Create(const ip: string; port: int);
begin
  inherited Create();
  con := TConnectionObjectTCP_OwnSocket.Create();
  con.Host := ip;
  con.Port := port;
end;

function TEmulationThread.PrepareBuffer(n_car, utime: int): ArrayOfByte;
begin
  Result := TextNumbersStringToArray('55 18 7A 03 E9 68 74 1C 00 00 00 00 00 00 00 00 12 00 F4 00 85 00 53 00 44 00 BB 00 16 00 72 07 76 01 00 00 00 00 06 00 00 00 00 00 00 00');
  WriteWORD(Result, 2, n_car); // N_Car
  if utime <= 0 then
    WriteUINT(Result, 4, NowGM()) // UTime
  else
    WriteUINT(Result, 4, utime);
end;

procedure TEmulationThread.SafeExecute;
begin
  while not Terminated do
  begin
    con.ExchangeBlockData(etSend);
    SleepThread(10000);
  end;
end;

procedure TForm1.EmulationThreadPackDone(Sender: TObject);
begin
  Memo1.Lines.Add('Pack done');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  thrEmulatePack := TEmulationThreadPack.Create('127.0.0.1', StrToInt(ePort.Text), 1, 1000);
  thrEmulatePack.OnTerminate := EmulationThreadPackDone;
end;

{ TEmulationThreadPack }

constructor TEmulationThreadPack.Create(const ip: string; port, ncar, count: int);
begin
  inherited Create(ip, port);
  FCount := count;
  FNCar := ncar;
  FreeOnTerminate := true;
end;

procedure TEmulationThreadPack.SafeExecute;
const packSize = 10;
var i, j: int;
    buf: ArrayOfByte;
    utime: LongWord;
begin
  for i := 0 to FCount div packSize do
  begin
    for j := 0 to packSize - 1 do
    begin
      utime := int64(NowGM()) - (i * packSize + j) * UTC_MINUTE;
      buf := PrepareBuffer(FNCar, utime);
      WriteBuf(con.buffers.BufSend, j * length(buf), buf, Length(buf));
    end;

    con.buffers.LengthSend := Length(buf) * packSize;
    con.ExchangeBlockData(etSend);
    Sleep(100);
  end;
end;

end.
