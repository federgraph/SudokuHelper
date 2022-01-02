unit SH.SudokuMain;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  FMX.Dialogs,
  SH.Interfaces,
  SH.SudokuGrid;

type
  TSudokuMain = class
  private
    ML: TStringList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn, filter: string): string;
    function GetSaveFileName(dn, fn, filter: string): string;
  public const
    DefaultMarkName = 'M0';
    DefaultSudokuBinFileName = 'MySudoku.sudoku';
    DefaultSudokuTxtFileName = 'MySudoku.txt';
    BinFilter = 'Sudoku-File|*.sudoku';
    TxtFilter = 'Sudoku-Text-File|*.txt';
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMark;
    procedure RevertToMark(mn: string);

    procedure SaveSudokuBin(fn: string = DefaultSudokuBinFileName);
    procedure LoadSudokuBin(fn: string = DefaultSudokuBinFileName);

    procedure SaveSudokuTxt(fn: string = DefaultSudokuTxtFileName);
    procedure LoadSudokuTxt(fn: string = DefaultSudokuTxtFileName);
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  System.IOUtils,
  SH.HelperBase,
  SH.Memory,
  SH.Strings,
  SH.SudokuFiler;

constructor TSudokuMain.Create;
begin
  ML := TStringList.Create;
end;

destructor TSudokuMain.Destroy;
begin
  ML.Free;
  inherited;
end;

procedure TSudokuMain.LoadSudokuBin(fn: string);
var
  LFilename: string;
  LSudoku: ISudokuHelper;
begin
  LFileName := GetOpenFileName(AppMemory.LastFolder, fn, BinFilter);
  if LFileName = '' then
    Exit;

  LSudoku := TSudokuFiler.LoadFromFile(LFilename);
  if Assigned(LSudoku) then
  begin
    Main.Sudoku := LSudoku;
    Main.InitializeSudoku;
    Main.Sudoku.Display.Refresh;
    AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  end;
end;

procedure TSudokuMain.RevertToMark(mn: string);
begin
  Main.Sudoku.RevertToMark(mn);
end;

procedure TSudokuMain.SaveSudokuBin(fn: string);
var
  LFilename: string;
begin
  LFileName := GetSaveFileName(AppMemory.LastFolder, fn, BinFilter);
  if LFileName = '' then
    Exit;

  TSudokuFiler.SaveToFile(Main.Sudoku, LFilename);
  AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
end;

procedure TSudokuMain.SetMark;
begin
  Main.Sudoku.AddMark(DefaultMarkName);
end;

function TSudokuMain.GetOpenFileName(dn, fn, filter: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(FormMain);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := filter;
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TSudokuMain.GetSaveFileName(dn, fn, filter: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(FormMain);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := filter;
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;

procedure TSudokuMain.SaveSudokuTxt(fn: string);
var
  LFilename: string;
begin
  LFileName := GetSaveFileName(AppMemory.LastFolder, fn, TxtFilter);
  if LFileName = '' then
    Exit;

  ML.Clear;
  ML.Add('SudokuType = ' + Main.Sudoku.Displayname);
  Main.Sudoku.Data.SaveToML(ML);
  ML.SaveToFile(LFileName);
  AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
end;

procedure TSudokuMain.LoadSudokuTxt(fn: string);
var
  LFilename: string;
  LName: string;
  LSudoku: ISudokuHelper;
begin
  LFileName := GetOpenFileName(AppMemory.LastFolder, fn, TxtFilter);
  if LFileName = '' then
    Exit;

  ML.LoadFromFile(LFileName);

  LName := ML.ValueFromIndex[0].Trim;
  ML.Delete(0);

  LSudoku := HelperRegistry.CreateInstance(LName);
  if Assigned(LSudoku) then
  begin
    Main.Sudoku := LSudoku;
    Main.Sudoku.Data.LoadFromML(ML);
    Main.InitializeSudoku;
    Main.Sudoku.Display.Refresh;
    AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  end;
end;

end.

