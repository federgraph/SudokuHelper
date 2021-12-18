unit SH.SudokuMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Dialogs,
  SH.Interfaces,
  SH.SudokuGrid;

type
  TSudokuMain = class
  strict private const
    DefaultSudokuFileName = 'MySudoku.sudoku';
  strict private
    FLastMarkNum: Integer;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  protected
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure HelpActionExecute(Sender: TObject);

    procedure LoadSudokuActionBeforeExecute(Sender: TObject);
    procedure SaveSudokuActionBeforeExecute(Sender: TObject);

    procedure StartNewActionExecute(Sender: TObject);

    procedure RevertToMarkActionExecute(MarkName: string);
    procedure RevertToMarkActionUpdate(Sender: TObject);
    procedure SetMarkActionExecute(Sender: TObject);

    procedure Display(const S: string; Timed: Boolean = false); overload;
    procedure Display(const Fmt: string; const A: array of const; Timed: Boolean = false); overload;
  public
    constructor Create;
    procedure LoadSudokuActionAccept(fn: string = DefaultSudokuFileName);
    procedure SaveSudokuActionAccept(fn: string = DefaultSudokuFileName);
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  System.IOUtils,
  SH.Memory,
  SH.Strings,
  SH.SudokuFiler;

constructor TSudokuMain.Create;
begin
  inherited;
//  AppMemory.RestoreFormState(self);
end;

procedure TSudokuMain.Display(const S: string; Timed: Boolean = false);
begin
//  if Statusbar.SimplePanel then
//    Statusbar.SimpleText := S
//  else if Statusbar.Panels.Count > 0 then
//    Statusbar.Panels[0].Text := S;
//  if Timed then
//  begin
//    MessageTimer.Interval := MessageTimeout;
//    MessageTimer.Enabled := true;
//  end;
end;

procedure TSudokuMain.Display(const Fmt: string; const A: array of const; Timed: Boolean = false);
begin
  Display(Format(Fmt, A), Timed);
end;

procedure TSudokuMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  AppMemory.SaveFormState(self);
end;

procedure TSudokuMain.HelpActionExecute(Sender: TObject);
begin
//  THelpViewerForm.Execute;
end;

procedure TSudokuMain.LoadSudokuActionAccept(fn: string);
var
  LFilename: string;
  LSudoku: ISudokuHelper;
begin
//  LFilename := LoadSudokuAction.Dialog.FileName;
  LFileName := GetOpenFileName(AppMemory.LastFolder, fn);
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

procedure TSudokuMain.LoadSudokuActionBeforeExecute(Sender: TObject);
begin
//  LoadSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TSudokuMain.RevertToMarkActionExecute(MarkName: string);
begin
  Main.Sudoku.RevertToMark(MarkName);
end;

procedure TSudokuMain.RevertToMarkActionUpdate(Sender: TObject);
begin
//  (Sender as TAction).Enabled := Sudoku.HasMarks;
end;

procedure TSudokuMain.SaveSudokuActionAccept(fn: string);
var
  LFilename: string;
begin
  LFileName := fn;
//  LFilename := SaveSudokuAction.Dialog.FileName;
  LFileName := GetSaveFileName(AppMemory.LastFolder, fn);
  if LFileName = '' then
    Exit;

  TSudokuFiler.SaveToFile(Main.Sudoku, LFilename);
  AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  Display(SSaveFileMessageMask, [LFilename], true);
end;

procedure TSudokuMain.SaveSudokuActionBeforeExecute(Sender: TObject);
begin
//  SaveSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TSudokuMain.SetMarkActionExecute(Sender: TObject);
var
  LMark: string;
begin
  { Generate a proposed name }
  repeat
    Inc(FLastMarkNum);
    LMark := String.Format(SNewMarkMask, [FLastMarkNum]);
  until not Main.Sudoku.MarkExists(LMark) ;

//  if InputQuery(SNewStackMarkCaption, SNewStackMarkPrompt, LMark) then
//  begin
    Main.Sudoku.AddMark(LMark);
//  end;
end;

procedure TSudokuMain.StartNewActionExecute(Sender: TObject);
var
  LSudokuName: string;
begin
  LSudokuName := 'Classic Sudoku (9x9)';
//  if TSelectSudokuDlg.Execute(LSudokuName, StartNewButton) then
  Main.CreateSudokuHelper(LSudokuName);
end;

function TSudokuMain.GetOpenFileName(dn, fn: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(FormMain);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := 'Sudoku-File|*.sudoku';
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TSudokuMain.GetSaveFileName(dn, fn: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(FormMain);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := 'Sudoku-File|*.sudoku';
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;


end.

