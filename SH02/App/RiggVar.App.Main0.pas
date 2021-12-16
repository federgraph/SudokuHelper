unit RiggVar.App.Main0;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  FMX.Layouts,
  RiggVar.FB.Action,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionItem,
  RiggVar.FB.ActionMap,
  RiggVar.FB.ActionKeys,
  RiggVar.FB.ActionTest,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionHelper,
  RiggVar.FB.ActionLong,
  RiggVar.FB.Touch,
  RiggVar.FederModel.Keyboard01,
  RiggVar.FederModel.Action,
  RiggVar.FederModel.TouchBase,
  RiggVar.FederModel.TouchPhone,
  RiggVar.FederModel.Touch,
  SH.Memory,
  SH.Strings,
  SH.Interfaces,
  SH.HelperBase,
  SH.SudokuGrid,
  SH.SudokuGraph;

type
  TSudokuHostForm = class(TInterfacedObject, ISudokuHostForm)
  private
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetRightClickAction: TRightClickAction;
    function GetModifierkeys: TShiftstate;
  public
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Modifierkeys: TShiftstate read GetModifierkeys;
    property RightClickAction: TRightClickAction read GetRightClickAction;
  end;

  TMain0 = class
  private
    FIsUp: Boolean;
    FTouch: Integer;
    FSudoku: ISudokuHelper;
    FCurrentValue: Integer;
    procedure InitFederText(ft: TFederTouch0);
    procedure InitRaster;
    function GetFederText: TFederTouchBase;
    function GetIsLandscape: Boolean;
    function GetIsPhone: Boolean;
    function GetIsPortrait: Boolean;
    function GetTouchbarLayout: Integer;
    function GetColorScheme: Integer;
    procedure InitText;
    procedure InitTouch;
    procedure InitializeSudoku;
    procedure SetIsUp(const Value: Boolean);
    procedure SetColorScheme(const Value: Integer);
    procedure SetTouch(const Value: Integer);
    procedure SetTouchbarLayout(const Value: Integer);
    procedure SetCurrentValue(const Value: Integer);
    function GetSetCandidatesButtonDown: Boolean;
    function GetToggleGosuButtonDown: Boolean;
    function GetUnsetCandidatesButtonDown: Boolean;
  public
    SudokuHostForm: ISudokuHostForm;
    BackgroundLock: Boolean;

    ActionHandler: TActionHelper;
    ActionGroupList: TActionGroupList;
    ActionTest: TActionTest;
    ActionItem: TActionItem;

    ActionMapTablet: TActionMap;
    ActionMapPhone: TActionMap;
    Keyboard: TFederKeyboard01;
    FederTextTablet: TFederTouch;
    FederTextPhone: TFederTouchPhone;

    SudokuGrid: TSudokuGrid;
    SudokuGraph: TSudokuGraph;

    ToggleGosuButtonEnabled: Boolean;

    RightClickAction: TRightClickAction;

    constructor Create;
    destructor Destroy; override;

    procedure CycleToolSet(i: Integer);
    procedure CycleColorSchemeM;
    procedure CycleColorSchemeP;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarTop(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure Init;
    procedure UpdateText;
    procedure UpdateTouch;
    procedure FederTextCheckState;
    procedure HandleAction(fa: TFederAction);
    procedure ExecuteAction(fa: Integer);
    function IsActionChecked(fa: Integer): Boolean;
    procedure CollectShortcuts(fa: Integer; ML: TStrings);
    procedure WriteHelpText(ML: TStrings);

    procedure InitFirstSudoku;
    procedure CreateSudokuHelper(const aName: string);
    procedure StartNew(fa: TFederAction);
    procedure HandleCharacter(AChar: Char);

    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
    property IsUp: Boolean read FIsUp write SetIsUp;
    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;
    property Touch: Integer read FTouch write SetTouch;
    property TouchbarLayout: Integer read GetTouchbarLayout write SetTouchbarLayout;
    property FederText: TFederTouchBase read GetFederText;

    property Sudoku: ISudokuHelper read FSudoku write FSudoku;
    property CurrentValue: Integer read FCurrentValue write SetCurrentValue;
    property ToggleGosuButtonDown: Boolean read GetToggleGosuButtonDown;
    property SetCandidatesButtonDown: Boolean read GetSetCandidatesButtonDown;
    property UnsetCandidatesButtonDown: Boolean read GetUnsetCandidatesButtonDown;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet,
  SH.SudokuHelper;

{ TSudokuHostForm }

function TSudokuHostForm.GetCurrentCandidate: TSudokuValue;
begin
  result := 1;
  if Main.IsUp then
    result := Main.CurrentValue;
end;

function TSudokuHostForm.GetCurrentValue: TSudokuValue;
begin
  result := 1;
  if Main.IsUp then
    result := Main.CurrentValue;
end;

function TSudokuHostForm.GetModifierkeys: TShiftState;
begin
  result := MainVar.ShiftState;
end;

function TSudokuHostForm.GetRightClickAction: TRightClickAction;
var
  LState: TShiftstate;
begin
  LState:= MainVar.ShiftState;
  if ssAlt in LState then
    Result := TRightClickAction.SetCandidate
  else if ssCtrl in LState then
    Result := TRightClickAction.UnsetCandidate
  else if Main.ToggleGosuButtonEnabled and Main.ToggleGosuButtonDown then
    Result := TRightClickAction.ToggleGosu
  else if Main.SetCandidatesButtonDown then
     Result := TRightClickAction.SetCandidate
  else if Main.UnsetCandidatesButtonDown then
    Result := TRightClickAction.UnsetCandidate
  else
    { default action is to set a candidate }
    Result := TRightClickAction.SetCandidate
end;

{ TMain0 }

constructor TMain0.Create;
begin
  Main := self;
  SudokuHostForm := TSudokuHostForm.Create;

  FCurrentValue := 1;
  FTouch := faTouchDesk;

  ActionMapTablet := TActionMapTablet.Create;
  ActionMapPhone := TActionMapPhone.Create;

  Keyboard := TFederKeyboard01.Create;

  InitRaster;

  TTouchBtn.WantHint := True;
  FederTextTablet := TFederTouch.Create(nil);
  FederTextPhone := TFederTouchPhone.Create(nil);

  ActionHandler := TActionHelper.Create;
  ActionGroupList := TActionGroupList.Create;
  ActionTest := TActionTest.Create;
  ActionItem := TActionItem.Create(faNoop);
end;

destructor TMain0.Destroy;
begin
  FederTextTablet.Free;
  FederTextPhone.Free;
  Keyboard.Free;
  ActionMapTablet.Free;
  ActionMapPhone.Free;
  ActionHandler.Free;
  ActionGroupList.Free;
  ActionTest.Free;
  ActionItem.Free;

  FSudoku := nil;
  SudokuHostForm := nil;

  inherited;
end;

procedure TMain0.HandleAction(fa: TFederAction);
begin
  case fa of
    faNoop: ;
    else
      FormMain.HandleAction(fa);
   end;
end;

procedure TMain0.ExecuteAction(fa: TFederAction);
begin
  ActionHandler.Execute(fa);
end;

procedure TMain0.FederTextCheckState;
begin
  FederText.CheckState;
end;

procedure TMain0.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
end;

procedure TMain0.Init;
begin
  InitRaster;
  InitText;
end;

procedure TMain0.InitFederText(ft: TFederTouch0);
begin
  if ft is TLayout then
  begin
    ft.Parent := FormMain;
    TFederTouchBase.OwnerComponent := ft;
    TFederTouchBase.ParentObject := ft;
  end
  else
  begin
    TFederTouchBase.OwnerComponent := FormMain;
    TFederTouchBase.ParentObject := FormMain;
  end;

  ft.Position.X := 0;
  ft.Position.Y := 0;
  ft.Width := MainVar.ClientWidth;
  ft.Height := MainVar.ClientHeight;
  ft.Init;
end;

procedure TMain0.InitRaster;
begin
  MainVar.ClientWidth := FormMain.ClientWidth;
  MainVar.ClientHeight := FormMain.ClientHeight;
end;

procedure TMain0.InitText;
begin
  InitRaster;
  InitFederText(FederTextTablet);
  InitFederText(FederTextPhone);
  Touch := faTouchDesk;
end;

procedure TMain0.InitTouch;
begin
  InitRaster;
  FederTextTablet.Visible := not IsPhone;
  FederTextPhone.Visible := IsPhone;
end;

function TMain0.IsActionChecked(fa: Integer): Boolean;
begin
  result := ActionHandler.GetChecked(fa);
end;

procedure TMain0.UpdateText;
begin
  FederText.UpdateText;
end;

procedure TMain0.UpdateTouch;
begin
  if Assigned(FederText) and FederText.InitOK then
  begin
    InitRaster;
    InitTouch;
    FederText.UpdateShape;
  end;
end;

function TMain0.GetFederText: TFederTouchBase;
begin
  case FTouch of
    faTouchTablet: result := FederTextTablet;
    faTouchPhone: result := FederTextPhone;
    faTouchDesk:
    begin
      if IsPhone then
        result := FederTextPhone
      else
        result := FederTextTablet;
    end;
    else
      result := FederTextTablet;
  end;
end;

function TMain0.GetIsLandscape: Boolean;
begin
  result := FormMain.ClientWidth > FormMain.ClientHeight;
end;

function TMain0.GetIsPhone: Boolean;
var
  MinCount, MaxCount: Integer;
begin
  case FTouch of
    faTouchPhone: result := True;
    faTouchTablet: result := False;
    else
    begin
      MinCount := Min(MainVar.ClientHeight, MainVar.ClientWidth) div MainVar.Raster;
      MaxCount := Max(MainVar.ClientHeight, MainVar.ClientWidth) div MainVar.Raster;
      result  := (MinCount < 8) or (MaxCount < 12);
    end;
  end;
end;

function TMain0.GetIsPortrait: Boolean;
begin
  result := not IsLandscape;
end;

function TMain0.GetTouchbarLayout: Integer;
begin
  if Assigned(FederText) then
    result := FederText.TouchBarLayout
  else
    result := 1;
end;

function TMain0.GetColorScheme: Integer;
begin
  result := MainVar.ColorScheme.CurrentScheme;
end;

procedure TMain0.SetColorScheme(const Value: Integer);
begin
  if not BackgroundLock then
  begin
    MainVar.ColorScheme.CurrentScheme := Value;
    MainVar.ColorScheme.Init(Value);
    FormMain.UpdateBackground;
    FederText.UpdateColorScheme;
  end;
end;

procedure TMain0.SetTouch(const Value: Integer);
begin
  FTouch := Value;
  if IsPhone then
    FederTextTablet.Visible := False
  else case FTouch of
    faTouchTablet: FederTextTablet.Visible := True;
    faTouchPhone: FederTextTablet.Visible := False;
  else
      FederTextTablet.Visible := not IsPhone;
  end;
  FederTextPhone.Visible := not FederTextTablet.Visible;

  FederText.UpdateShape;
end;

procedure TMain0.SetTouchbarLayout(const Value: Integer);
begin
  if Assigned(FederText) then
    FederText.TouchBarLayout := Value;
end;

procedure TMain0.DoTouchbarLeft(Delta: single);
begin

end;

procedure TMain0.DoTouchbarTop(Delta: single);
begin

end;

procedure TMain0.DoTouchbarRight(Delta: single);
var
  d: single;
begin
  if Delta > 0 then
    d := 1
  else
    d := -1;
  DoBigWheel(-d);
end;

procedure TMain0.DoTouchbarBottom(Delta: single);
var
  d: single;
begin
  if Delta > 0 then
    d := 1
  else
    d := -1;
  DoSmallWheel(-d);
end;

procedure TMain0.CycleToolSet(i: Integer);
begin
  FederText.UpdateToolSet(i);
end;

procedure TMain0.CycleColorSchemeM;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Dec(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.CycleColorSchemeP;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Inc(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.HandleCharacter(AChar: Char);
begin
  Sudoku.InputHandler.HandleCharacter(SudokuGrid.Col, SudokuGrid.Row, AChar);
end;

procedure TMain0.InitFirstSudoku;
begin
//  CreateSudokuHelper(AppMemory.LastSudoku);
  CreateSudokuHelper(GetFederActionLong(faSudoku09A));
end;

procedure TMain0.CreateSudokuHelper(const aName: string);
begin
  Sudoku := HelperRegistry.CreateInstance(aName);
  InitializeSudoku;
end;

procedure TMain0.InitializeSudoku;
begin
  SudokuGraph.DataStorage := Sudoku.Data;
  Sudoku.Display.InitializeGrid(SudokuGraph);
  Sudoku.InputHandler.Initialize(SudokuHostForm);
  ToggleGosuButtonEnabled := Sudoku.IsGosu;
  AppMemory.LastSudoku := Sudoku.Displayname;
  FormMain.Caption := string.Format(SMainformCaptionMask, [Sudoku.Displayname]);
end;

procedure TMain0.StartNew(fa: TFederAction);
var
  s: string;
begin
  s := ActionHandler.GetCaption(fa);
  CreateSudokuHelper(s);
end;

procedure TMain0.SetCurrentValue(const Value: Integer);
begin
  if Value <= SudokuGrid.ColCount then
  begin
    FCurrentValue := Value;
    FederText.ST00.Caption := IntToStr(Value);
  end;
end;

function TMain0.GetSetCandidatesButtonDown: Boolean;
begin
  result := RightClickAction = TRightClickAction.SetCandidate;
end;

function TMain0.GetToggleGosuButtonDown: Boolean;
begin
  result := RightClickAction = TRightClickAction.ToggleGosu;
end;

function TMain0.GetUnsetCandidatesButtonDown: Boolean;
begin
  result := RightClickAction = TRightClickAction.UnsetCandidate;
end;

procedure TMain0.DoBigWheel(Delta: single);
begin
  SudokuGrid.NavRow(Round(Delta));
end;

procedure TMain0.DoSmallWheel(Delta: single);
begin
  SudokuGrid.NavCol(Round(Delta));
end;

procedure TMain0.CollectShortcuts(fa: Integer; ML: TStrings);
begin
  Keyboard.GetShortcuts(fa, ML);
//  ActionMap0.CollectOne(fa, ML);
{$ifdef WantFederText}
  ActionMapTablet.CollectOne(fa, ML);
  ActionMapPhone.CollectOne(fa, ML);
{$endif}
//  FederMenu.CollectOne(fa, ML);
end;

procedure TMain0.WriteHelpText(ML: TStrings);
begin
  ML.Add('# SudokuHelper Readme');
  ML.Add('');
  ML.Add('Copyright © 2021 by Dr. Peter Below,');
  ML.Add('adapted for SH02 by Federgraph, see forked GitHub repository.');
  ML.Add('');
  ML.Add('SudokuHelper is an application that acts like an electronic Sudoku grid.');
  ML.Add('');
  ML.Add('It supports 9x9, 12x12, and 16x16 Sudokus,');
  ML.Add('  both in the classic and Gosu variant,');
  ML.Add('  where cells can be marked to only accept even numbers.');
  ML.Add('');
  ML.Add('The application neither creates Sudokus itself nor provides a solver for them;');
  ML.Add('  it is just a more convenient way to solve a Sudoku from a magazine');
  ML.Add('  or other external source than doing it on paper,');
  ML.Add('  using pencil and eraser.');
  ML.Add('');
  ML.Add('The application''s main features are:');
  ML.Add('- Invalid cell values are marked in red. ');
  ML.Add('- Candidate values can be added and removed from a cell.');
  ML.Add('- All actions can be undone, the undo stack is only limited by available memory. ');
  ML.Add('- Named marks can be set for the current undo stack state.');
  ML.Add('- The Sudoku can be saved to file, including the undo stack.');
  ML.Add('');
  ML.Add('## Basics of operation');
  ML.Add('');
  ML.Add('### Navigating the Grid with keyboard');
  ML.Add('');
  ML.Add('The active cell is marked in yellow, or blue for a Gosu cell.');
  ML.Add('');
  ML.Add('To navigate around the grid use the cursor keys to move one cell up, down, left or right.');
  ML.Add('');
  ML.Add('In addition:');
  ML.Add('- HOME moves to the first cell in the row.');
  ML.Add('- END moves to the last cell in the row.');
  ML.Add('- PageUp moves to the top cell in the column.');
  ML.Add('- PageDown moves to the bottom cell in the column.');
  ML.Add('');
  ML.Add('### Setting cell values with keyboard');
  ML.Add('');
  ML.Add('To set a cell''s value just type the value.');
  ML.Add('- 0 to clear the cell,');
  ML.Add('- 1 to 9 to set the cell value.');
  ML.Add('');
  ML.Add('For 12x12 and 16x16 Sudokus');
  ML.Add('  the letters A to G will set the values 10 to 16.');
  ML.Add('');
  ML.Add('### How to toggle Even-Values-Only cell state');
  ML.Add('');
  ML.Add('For Gosu-type Sudokus Spacebar should toggle');
  ML.Add('  the active cells Even-Values-Only state.');
  ML.Add('');
  ML.Add('### Setting candidates with keyboard');
  ML.Add('');
  ML.Add('To set a candidate, hold down the Alt key while typing.');
  ML.Add('To remove a candidate, use the Ctrl key instead.');
  ML.Add('Candidates can only be set on an empty cell.');
  ML.Add('');
  ML.Add('### Setting values with mouse');
  ML.Add('');
  ML.Add('Using the left mouse button,');
  ML.Add('  first click on one of the numbered buttons to SELECT the value,');
  ML.Add('  then click on a cell in the grid to PLACE the value.');
  ML.Add('');
  ML.Add('Using 0 as value should clear the cell.');
  ML.Add('');
  ML.Add('### Setting candidates with mouse');
  ML.Add('');
  ML.Add('For candidates it should go like this:');
  ML.Add('- make sure to select the correct value,');
  ML.Add('- make sure the appropriate mode of operation is set,');
  ML.Add('- then click on an empty cell with the right mouse button.');
  ML.Add('');
  ML.Add('Right mouse button clicks should work together with the modifier keys');
  ML.Add('  Alt (set a candidate) and');
  ML.Add('  Ctrl (remove a candidate).');
  ML.Add('');
  ML.Add('The currently active mode should be reflected in the button''s down state.');
  ML.Add('');
  ML.Add('## Sudoku commands');
  ML.Add('');
  ML.Add('[Clear stack] action should discard all items on the Undo stack,');
  ML.Add('  including all stack marks.');
  ML.Add('//The action should only be enabled if the stack is not empty.');
  ML.Add('');
  ML.Add('[Undo] action should undo the last user action that changed the Sudoku''s content,');
  ML.Add('  including the candidates.');
  ML.Add('//This action should be enabled only if the stack is not empty.');
  ML.Add('');
  ML.Add('Not implemented yet:');
  ML.Add('');
  ML.Add('[New Sudoku] shows a list of the Sudoku types the application can handle.');
  ML.Add('  Select the one you want and click OK.');
  ML.Add('  A new empty Sudoku is created and both grid and value buttons are adjusted as needed. ');
  ML.Add('');
  ML.Add('[Save Sudoku] should bring up a File Save dialog.');
  ML.Add('  It should remember the last folder you saved a Sudoku to, or loaded one from.');
  ML.Add('  Enter a filename and click the dialog''s Save button to store the current Sudoku,');
  ML.Add('  including the Undo stack, to the file. ');
  ML.Add('');
  ML.Add('[Load Sudoku] should bring up a File Open dialog.');
  ML.Add('  It should remember the last folder you saved a Sudoku to or loaded one from.');
  ML.Add('  Pick a filename and click the dialog''s Open button to replace the current Sudoku,');
  ML.Add('  including the Undo stack, with the one saved to the file. ');
  ML.Add('');
  ML.Add('[Set Mark] should pop up a simple dialog where you can enter a name for the mark to create.');
  ML.Add('  It then represents the current state of the undo stack. ');
  ML.Add('');
  ML.Add('[Revert to Mark] should pop up a list of the defined stack marks.');
  ML.Add('  Select the one you want to revert to and click OK.');
  ML.Add('  The application should then undo all changes done after the mark was set.');
  ML.Add('  This action should only be enabled if there is at least one stack mark defined.');
end;

end.
