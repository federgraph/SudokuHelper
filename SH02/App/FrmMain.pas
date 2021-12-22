unit FrmMain;

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
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.Graphics,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ColorScheme,
  RiggVar.FD.Image,
  SH.SudokuGraph;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    Raster: single;
    Margin: single;
    ComponentsCreated: Boolean;
    FormShown: Boolean;

    SudokuImage: TOriginalImage;
    SudokuGraph: TSudokuGraph;

    procedure HandleShowHint(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure SudokuImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    function GetIsUp: Boolean;
    procedure SetIsUp(const Value: Boolean);
    procedure Init;
    procedure InitSudokuGraph;
    procedure CreateComponents;
    procedure DestroyForms;

    procedure ActionsBtnClick(Sender: TObject);
    procedure MemoBtnClick(Sender: TObject);
    procedure FixZOrder;
    procedure OriginalZOrder;

    property IsUp: Boolean read GetIsUp write SetIsUp;
  public
    procedure UpdateBackground;
    procedure HandleAction(fa: TFederAction);
    procedure AddToDebugText(ML: TStrings);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FrmAction,
  FrmMemo,
  RiggVar.App.Main;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormMain := self;

  ReportMemoryLeaksOnShutdown := True;
  FormatSettings.DecimalSeparator := '.';

  Fill.Kind := TBrushKind.Solid; // because it is still TBrushKind.None
  Self.Position := TFormPosition.ScreenCenter;

  ClientWidth := 1024;
  ClientHeight := 900;

  Init;

  CreateComponents;

  Application.OnHint := HandleShowHint;
  OnShow := FormShow;
end;

procedure TFormMain.Init;
begin
  Main := TMain.Create;
  Main.Init;
  IsUp := True;

  Main.Keyboard.KeyMapping := 1;
  Main.FederText.ActionPage := 1;
  Main.ColorScheme := 4;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MainVar.AppIsClosing := True;

  SudokuGraph.Free;

  DestroyForms;
  Main.Free;
  Main := nil;
end;

procedure TFormMain.DestroyForms;
begin
  if FormAction <> nil then
  begin
    FormAction.DisposeOf;
    FormAction := nil;
  end;
  if FormMemo <> nil then
  begin
    FormMemo.DisposeOf;
    FormMemo := nil;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    Main.InitFirstSudoku;
    SudokuGraph.DrawNeeded := True;
    Application.OnIdle := ApplicationEventsIdle;
  end;
end;

procedure TFormMain.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if IsUp then
  begin
    if SudokuGraph.DrawNeeded then
      SudokuGraph.Draw;
  end;
  Done := True;
end;

procedure TFormMain.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Memo.Lines.Clear;
    Main.WriteHelpText(FormMemo.Memo.Lines);
  end;
  FormMemo.Visible := True;
end;

procedure TFormMain.ActionsBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
  end;
  FormAction.Visible := True;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if IsUp then
  begin
    Main.UpdateTouch;
    Main.UpdateText;
    SudokuGraph.Invalidate;
  end;
end;

function TFormMain.GetIsUp: Boolean;
begin
  if Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.UpdateBackground;
begin
  Self.Fill.Color := MainVar.ColorScheme.claBackground;
end;

procedure TFormMain.InitSudokuGraph;
var
  t: Integer;
begin
  t := 1024;
  SudokuImage := TOriginalImage.Create(Self, t, t);
  SudokuImage.Name := 'SudokuImage';
  SudokuImage.Parent := Self;
  SudokuImage.Position.X := Raster + Margin;
  SudokuImage.Position.Y := Raster + Margin;
  SudokuImage.HitTest := False;
  SudokuImage.Visible := False;

  SudokuGraph := TSudokuGraph.Create;
  SudokuGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
  SudokuGraph.ImageOpacity := 0.2;

  SudokuGraph.Image := SudokuImage;
end;

procedure TFormMain.HandleAction(fa: TFederAction);
begin
  case fa of
    faShowActions: ActionsBtnClick(nil);
    faShowMemo: MemoBtnClick(nil);

    faTL02: FixZOrder;
    faTL03: OriginalZOrder;
    faTL04: SudokuGraph.WantClearToRed := not SudokuGraph.WantClearToRed;
  end;
end;

procedure TFormMain.CreateComponents;
begin
  Raster := MainVar.Raster;
  Margin := 10.0;

  InitSudokuGraph;
  Main.SudokuGraph := SudokuGraph;
  SudokuImage.Visible := True;
  FixZOrder;

  ComponentsCreated := True;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  Main.FederText.SB00.Caption := Application.Hint;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  MainVar.ShiftState := Shift;
  if IsUp then
    Main.ActionHandler.FormKeyUp(Sender, Key, KeyChar, Shift);
end;

procedure TFormMain.SudokuImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LCol: single;
  LRow: single;
begin
  LCol := X / (SudokuGraph.DefaultColWidth * SudokuGraph.Zoom);
  if LCol >= SudokuGraph.ColCount then
    Exit;

  LRow := Y / (SudokuGraph.DefaultRowHeight * SudokuGraph.Zoom);
  if LRow >= SudokuGraph.RowCount then
    Exit;

  SudokuGraph.Col := Floor(LCol);
  SudokuGraph.Row := Floor(LRow);
  Main.Sudoku.InputHandler.HandleCellClick(SudokuGraph.Col, SudokuGraph.Row, Main.ClickAction);
end;

procedure TFormMain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MainVar.ShiftState := Shift;
  SudokuImageMouseUp(Sender, Button, Shift,
  X - SudokuImage.Position.X,
  Y - SudokuImage.Position.Y);
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  delta: Integer;
begin
  MainVar.ShiftState := Shift;
  if IsUp then
  begin
    if WheelDelta > 0 then
      delta := 1
    else
      delta := -1;

    if ssShift in Shift then
      Main.DoBigWheel(-delta)
    else
      Main.DoSmallWheel(delta);
  end;
end;

procedure TFormMain.AddToDebugText(ML: TStrings);
var
  i: Integer;
  o: TFMXObject;
  c: TControl;
begin
  ML.Add('');
  ML.Add(Format('FormMain.Handle.Scale = %.1f', [Handle.Scale]));
  ML.Add('');
  ML.Add('FormMain.ZOrderInfo:');
  for i := 0 to Self.ChildrenCount-1 do
  begin
    o := Self.Children.Items[i];
    if o is TControl then
    begin
      c := o as TControl;
      ML.Add(Format('%2d - %s: %s', [i, c.Name, c.ClassName]));
    end;
  end;
end;

procedure TFormMain.FixZOrder;
begin
  SudokuImage.BringToFront;
  Main.FederTextPhone.BringToFront;
  Main.FederTextTablet.BringToFront;

  SudokuImage.HitTest := True;
  SudokuImage.OnMouseUp := SudokuImageMouseUp;
  Self.OnMouseUp := nil;
end;

procedure TFormMain.OriginalZOrder;
begin
  Main.FederTextPhone.BringToFront;
  Main.FederTextTablet.BringToFront;
  SudokuImage.BringToFront;

  SudokuImage.HitTest := True;
  SudokuImage.OnMouseUp := nil;
  Self.OnMouseUp := FormMouseUp;
end;

end.
