{!
<summary>
 This unit contains the singleton managing the app's settings.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-16<p>
 Last modified by PB 2021-11-17<p>
 Last modified by GS 2022-01-xx<p>
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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Sysutils,
  Forms;

type
  IAppMemory = interface(IInterface)
  ['{629ECBCD-CE8D-4A16-95F6-2363B967462B}']
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
  FileUtil,
  LazFileUtils,
  Inifiles,
  Registry,
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
  LAppname:= ExtractFileNameWithoutExt(ParamStr(0));
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
  dd := AppendPathDelim(GetUserDir + 'Documents');
  Result := Memory.ReadString(CSettings, CLastFolder,  Concat(dd, CDataFolder));
end;

function TAppMemory.GetLastSudoku: string;
begin
  Result := Memory.ReadString(CSettings, CLastSudokuKey, CClassicSudoku9x9);
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
