unit ConnParamsStorage;

interface

type
  TZConnectionParams = record
    Port: integer;
    Host: string;
    Login: string;
    Password: string;
    Database: string;
    LibraryLocation: string;
  end;

procedure SetGlobalSQLConnectionParams(ConnectionParams: TZConnectionParams);
function GlobalSQLConnectionParams(): TZConnectionParams;

implementation

var SQLConnectionParams: TZConnectionParams;

procedure SetGlobalSQLConnectionParams(ConnectionParams: TZConnectionParams);
begin
  SQLConnectionParams := ConnectionParams;
end;

function GlobalSQLConnectionParams(): TZConnectionParams;
begin
  Result := SQLConnectionParams;
end;

end.
