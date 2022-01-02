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
  SH.SudokuGraph,
  SH.SudokuHelper,
  SH.SudokuMain;

type
  TSudokuHostForm = class(TInterfacedObject, ISudokuHostForm)
  private
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetClickAction: TClickAction;
    function GetModifierkeys: TShiftstate;
  public
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Modifierkeys: TShiftstate read GetModifierkeys;
    property ClickAction: TClickAction read GetClickAction;
  end;

  TMain0 = class
  private
    FIsUp: Boolean;
    FTouch: TFederAction;
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
    procedure SetIsUp(const Value: Boolean);
    procedure SetColorScheme(const Value: Integer);
    procedure SetTouch(const Value: TFederAction);
    procedure SetTouchbarLayout(const Value: Integer);
    procedure SetCurrentValue(const Value: Integer);
    function GetSetCandidatesButtonDown: Boolean;
    function GetToggleGosuButtonDown: Boolean;
    function GetUnsetCandidatesButtonDown: Boolean;
    procedure SetClickAction(const Value: TClickAction);
    function GetClickAction: TClickAction;
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

    SudokuGraph: TSudokuGraph; // injected, not owned
    SudokuMain: TSudokuMain;

    FixedZOrder: Boolean;
    ToggleGosuButtonEnabled: Boolean;

    FClickAction: TClickAction;

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
    procedure ExecuteAction(fa: TFederAction);
    function IsActionChecked(fa: TFederAction): Boolean;
    procedure CollectShortcuts(fa: TFederAction; ML: TStrings);
    procedure RunTest01(ML: TStrings);

    procedure InitializeSudoku;
    procedure InitFirstSudoku;
    procedure CreateSudokuHelper(const aName: string);
    procedure StartNew(fa: TFederAction);
    procedure HandleCharacter(AChar: Char);

    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
    property IsUp: Boolean read FIsUp write SetIsUp;
    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;
    property Touch: TFederAction read FTouch write SetTouch;
    property TouchbarLayout: Integer read GetTouchbarLayout write SetTouchbarLayout;
    property FederText: TFederTouchBase read GetFederText;

    property Sudoku: ISudokuHelper read FSudoku write FSudoku;
    property CurrentValue: Integer read FCurrentValue write SetCurrentValue;
    property ToggleGosuButtonDown: Boolean read GetToggleGosuButtonDown;
    property SetCandidatesButtonDown: Boolean read GetSetCandidatesButtonDown;
    property UnsetCandidatesButtonDown: Boolean read GetUnsetCandidatesButtonDown;
    property ClickAction: TClickAction read GetClickAction write SetClickAction;
  end;

implementation

uses
  System.Generics.Collections,
  FrmMain,
  RiggVar.App.Main,
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet;

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

function TSudokuHostForm.GetClickAction: TClickAction;
begin
  Result := Main.ClickAction;
end;

{ TMain0 }

constructor TMain0.Create;
begin
  Main := self;
  SudokuHostForm := TSudokuHostForm.Create;
  SudokuMain := TSudokuMain.Create;

  FClickAction := TClickAction.SetValue;
  FCurrentValue := 1;
  FTouch := faTouchDesk;

  ActionMapTablet := TActionMapTablet.Create;
  ActionMapPhone := TActionMapPhone.Create;

  Keyboard := TFederKeyboard01.Create;
  Keyboard.TestName := 'Keyboard';

  InitRaster;

  TTouchBtn.WantHint := True;
  FederTextTablet := TFederTouch.Create(nil);
  FederTextTablet.Name := 'FederTextTablet';
  FederTextPhone := TFederTouchPhone.Create(nil);
  FederTextPhone.Name := 'FederTextPhone';

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
  SudokuMain.Free;

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

procedure TMain0.InitRaster;
begin
  MainVar.ClientWidth := FormMain.ClientWidth;
  MainVar.ClientHeight := FormMain.ClientHeight;
end;

procedure TMain0.Init;
begin
  InitText;
end;

procedure TMain0.InitText;
begin
  InitRaster;
  InitFederText(FederTextTablet);
  InitFederText(FederTextPhone);
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

procedure TMain0.InitTouch;
begin
  InitRaster;
  FederTextTablet.Visible := not IsPhone;
  FederTextPhone.Visible := IsPhone;
end;

function TMain0.IsActionChecked(fa: TFederAction): Boolean;
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
  result := MainVar.ClientWidth > MainVar.ClientHeight;
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

function TMain0.GetClickAction: TClickAction;
begin
  Result := FClickAction;
end;

function TMain0.GetColorScheme: Integer;
begin
  result := MainVar.ColorScheme.CurrentScheme;
end;

procedure TMain0.SetClickAction(const Value: TClickAction);
begin
  FClickAction := Value;
  if not ToggleGosuButtonEnabled and (Value = TClickAction.ToggleGosu) then
  begin
    FClickAction := TClickAction.SetFocus;
  end
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

procedure TMain0.SetTouch(const Value: TFederAction);
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
  Sudoku.InputHandler.HandleCharacter(SudokuGraph.Col, SudokuGraph.Row, AChar);
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

  FCurrentValue := 1;
  FederText.ST00.Caption := IntToStr(FCurrentValue);
  FederText.ActionPage := 1;
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
  if Value <= SudokuGraph.ColCount then
  begin
    FCurrentValue := Value;
    FederText.ST00.Caption := IntToStr(Value);
  end;
end;

function TMain0.GetSetCandidatesButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.SetCandidate;
end;

function TMain0.GetToggleGosuButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.ToggleGosu;
end;

function TMain0.GetUnsetCandidatesButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.UnsetCandidate;
end;

procedure TMain0.DoBigWheel(Delta: single);
begin
  SudokuGraph.NavRow(Round(Delta));
end;

procedure TMain0.DoSmallWheel(Delta: single);
begin
  SudokuGraph.NavCol(Round(Delta));
end;

procedure TMain0.CollectShortcuts(fa: TFederAction; ML: TStrings);
begin
  Keyboard.GetShortcuts(fa, ML);
  ActionMapTablet.CollectOne(fa, ML);
  ActionMapPhone.CollectOne(fa, ML);
//  FederMenu.CollectOne(fa, ML);
end;

procedure TMain0.RunTest01(ML: TStrings);
var
  LList: TStack<Integer>;
  I: Integer;
  LArray: TArray<Integer>;
  SB: TStringbuilder;
begin
  SB := TStringBuilder.Create(4096);
  try
    LList := TStack<Integer>.Create();
    try
      for I := 1 to 10 do
        LList.Push(I);
      SB.AppendLine('Enumerator sequence:');
      for I in LList do
        SB.AppendFormat('%d, ',[I]);
      SB.AppendLine;
      SB.AppendLine('ToArray sequence:');
      LArray:= LList.ToArray;
      for I := Low(LArray) to High(LArray) do
        SB.AppendFormat('%d, ',[LArray[I]]);
      SB.AppendLine;
      SB.AppendLine('Pop sequence:');
      while LLIst.Count >0 do
        SB.AppendFormat('%d, ',[LList.Pop]);
      SB.AppendLine;
      ML.Text := SB.ToString;
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

end.
