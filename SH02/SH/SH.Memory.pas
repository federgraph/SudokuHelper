﻿{!
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
    {! Getter for  the LastFolder property  }
    function GetLastFolder: string;
    {! Getter for the LastSudoku property  }
    function GetLastSudoku: string;
    {! Getter for  the LastFolder property  }
    procedure SetLastFolder(const Value: string);
    {! Setter for  the LastSudoku property  }
    procedure SetLastSudoku(const Value: string);
    {!
    <value>
     Last folder used to save a Sudoku to or load it from
    </value>}
    property LastFolder: string read GetLastFolder write SetLastFolder;
    {!
    <value>
     Display name of the last Sudoku type used
    </value>}
    property LastSudoku: string read GetLastSudoku write SetLastSudoku;

  end;

function AppMemory: IAppMemory;

implementation

uses
  System.Inifiles,
  System.IOUtils,
  System.RegularExpressions,
  System.Win.Registry,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionLong,
  PB.CharactersU,
  PB.CommonTypesU,
  PB.InterlockedOpsU,
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
var
  P: TObject;
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
    Result._AddRef;
    { the call below does not increment the refcount! }
    P := InterlockedCompareExchangeObject(InternalAppMemory, TObject(Pointer(Result)), nil);
    if P <> nil then
    begin
      Result._Release;
      Result := InternalAppMemory;
    end;
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
begin
  Result := Memory.ReadString(CSettings, CLastFolder,
    TPath.Combine(TPath.GetDocumentsPath, CDataFolder));
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
  InternalShutDownInProgress := true;
  InternalAppMemory := nil;

end.
