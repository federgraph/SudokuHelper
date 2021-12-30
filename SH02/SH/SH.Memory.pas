{!
<summary>
 This unit contains the singleton managing the app's settings.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-16<p>
 Last modified       2021-11-17<p>
</history>
<remarks>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
</remarks>
}
unit SH.Memory;

interface

uses
  System.Sysutils,
  FMX.Forms;

type
  IAppMemory = interface(IInterface)
  ['{4F4D7EB1-A25E-4D76-BC0A-D03BEC95E751}']
    function GetLastFolder: string;
    function GetLastSudoku: string;

    procedure SetLastFolder(const Value: string);
    procedure SetLastSudoku(const Value: string);

    property LastFolder: string read GetLastFolder write SetLastFolder;
    property LastSudoku: string read GetLastSudoku write SetLastSudoku;
  end;

function AppMemory: IAppMemory;

implementation

uses
  System.Inifiles,
  System.IOUtils,
  System.Win.Registry,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionLong,
  SH.SudokuHelper;

const
  CDataFolder = 'SudokuHelper';
  CLastFolder = 'LastFolder';
  CLastSudokuKey = 'LastSudoku';
  CSettings = 'Settings';
  CAppRegKey = 'Software\PeterBelow\SudokuHelper';

type
  TAppMemory = class(TInterfacedObject, IAppMemory)
  strict private
    FMemory: TCustomInifile;
    function GetLastFolder: string;
    function GetLastSudoku: string;
    procedure SetLastFolder(const Value: string);
    procedure SetLastSudoku(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Memory: TCustomInifile read FMemory;
  end;

var
  InternalAppMemory: IAppMemory = nil;
  InternalShutDownInProgress: Boolean = false;

function AppMemory: IAppMemory;
begin
  if InternalShutDownInProgress then
  begin
    Result := nil;
    Assert(false, 'AppMemory referenced during shutdown!');
  end
  else
  if Assigned(InternalAppMemory) then
    Result := InternalAppMemory
  else
  begin
    Result := TAppMemory.Create;
    InternalAppMemory := Result;
  end;
end;

constructor TAppMemory.Create;
var
  LAppname: string;
begin
  inherited;
  LAppname:= TPath.GetFileNameWithoutExtension(Paramstr(0));
  FMemory := TRegistryIniFile.Create(CAppRegKey + LAppname)
end;

destructor TAppMemory.Destroy;
begin
  FMemory.Free;
  inherited;
end;

function TAppMemory.GetLastFolder: string;
var
  dd: string;
begin
  dd := TPath.GetDocumentsPath;
  Result := Memory.ReadString(CSettings, CLastFolder, TPath.Combine(dd, CDataFolder));
end;

function TAppMemory.GetLastSudoku: string;
var
  DefaultSudokuName: string;
begin
  DefaultSudokuName := GetFederActionLong(faSudoku09A);
  Result := Memory.ReadString(CSettings, CLastSudokuKey, DefaultSudokuName);
end;

procedure TAppMemory.SetLastFolder(const Value: string);
begin
  Memory.WriteString(CSettings, CLastFolder, Value );
end;

procedure TAppMemory.SetLastSudoku(const Value: string);
begin
  if not Value.IsEmpty then
    Memory.WriteString(CSettings, CLastSudokuKey, Value);
end;

initialization
finalization
  InternalShutDownInProgress := True;
  InternalAppMemory := nil;

end.
