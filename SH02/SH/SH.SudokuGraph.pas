unit SH.SudokuGraph;

interface

uses
  RiggVar.FD.Image,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects,
  SH.DataStorageBase,
  SH.SudokuGrid,
  SH.Interfaces;

type
  TSudokuGraph = class(TSudokuGrid)
  private const
    Opaque = 1.0;
    Translucent = 0.8;
    NoCorners: TCorners = [];
  private var
    procedure ClearBackground(g: TCanvas);
  private
    FImage: TOriginalImage; // injected, not owned
    procedure InitBitmap;
    procedure SetImage(const Value: TOriginalImage);
  private
    SavedMatrix: TMatrix;
    NewMatrix: TMatrix;
    TempMatrix: TMatrix;
    procedure BeginTransform(g: TCanvas);
    procedure EndTransform(g: TCanvas);
  private
    procedure DrawToCanvas(g: TCanvas);
    procedure DrawSudokuGrid(g: TCanvas);
  private
    DrawCounter: Integer;
    FZoom: single;

    function GetSymbol(aValue: TSudokuValue): string;

    procedure DrawCellBorders(g: TCanvas; aCell: ISudokuCell; var Rect: TRectF; ASelected: Boolean);
    procedure DrawCellData(g: TCanvas; aCell: ISudokuCell; Rect: TRectF; ASelected: Boolean);
    procedure DrawCell(g: TCanvas; ACell: ISudokuCell; ACol, ARow: Integer; Rect: TRectF);
  public
    BackgroundColor: TAlphaColor;
    ImageOpacity: single;

    DataStorage: ISudokuData; // injected

    constructor Create;

    procedure Draw; override;

    property Zoom: single read FZoom;
    property Image: TOriginalImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main,
  FMX.Grid;

constructor TSudokuGraph.Create;
begin
  inherited;
  BackgroundColor := claAntiquewhite;
  ImageOpacity := 1.0;
end;

procedure TSudokuGraph.InitBitmap;
begin
  Image.Width := 16 * 64; // big enough to display largest sudoku
  Image.Height := Image.Width;
end;

procedure TSudokuGraph.SetImage(const Value: TOriginalImage);
begin
  FImage := Value;
  InitBitmap;
end;

procedure TSudokuGraph.BeginTransform(g: TCanvas);
var
  OriginX: single;
  OriginY: single;
  ss: single;
  TestSizeW: single;
  TestSizeH: single;
  TestSize: single;
begin
  OriginX := 0;
  OriginY := 0;

  TestSizeH := MainVar.ClientHeight - 160;
  TestSizeW := MainVar.ClientWidth - 160;
  TestSize := Min(TestSizeH, TestSizeW);
  TestSize := Max(TestSize, 70);

  if Width > TestSize then
    FZoom := TestSize / Width
  else
    FZoom := 1;

  ss := Image.Scene.GetSceneScale;

  SavedMatrix := g.Matrix;
  NewMatrix := TMatrix.Identity;

  TempMatrix := TMatrix.CreateScaling(FZoom * ss, FZoom * ss);
  NewMatrix := NewMatrix * TempMatrix;
  TempMatrix := TMatrix.CreateTranslation(OriginX * ss, OriginY * ss);
  NewMatrix := NewMatrix * TempMatrix;

  g.SetMatrix(NewMatrix);
end;

procedure TSudokuGraph.EndTransform(g: TCanvas);
begin
  g.SetMatrix(SavedMatrix);
end;

procedure TSudokuGraph.DrawToCanvas(g: TCanvas);
begin
  Inc(DrawCounter);
  if g.BeginScene then
  try
    BeginTransform(g);
    try
      ClearBackground(g);
      DrawSudokuGrid(g);
    finally
      EndTransform(g);
    end;
  finally
    g.EndScene;
  end;
end;

procedure TSudokuGraph.Draw;
begin
  if (Image <> nil) then
  begin
    DrawToCanvas(Image.Bitmap.Canvas);
    Image.Repaint;
  end;
end;

procedure TSudokuGraph.ClearBackground(g: TCanvas);
var
  R: TRectF;
begin
  g.Clear(claNull);
  if Image = nil then
  begin
    g.Clear(BackgroundColor);
  end
  else
  begin
    R := RectF(0, 0, Width, Height);
    g.Fill.Color := BackgroundColor;
    g.FillRect(R, 0, 0, [], ImageOpacity);
  end;
end;

procedure TSudokuGraph.DrawCellBorders(g: TCanvas; aCell: ISudokuCell; var Rect: TRectF; ASelected: Boolean);
var
  LLocation: TCellInBlockLocation;
begin
  g.Stroke.Color := claBlack;
  g.StrokeThickness := 1.0;
  g.StrokeCap := TStrokeCap.Flat;
  g.StrokeJoin := TStrokeJoin.Miter;
  if ASelected then
  begin
    if aCell.EvenOnly then
      g.Fill.Color := claAqua
    else
      g.Fill.Color := claYellow;
  end
  else if aCell.EvenOnly then
    g.Fill.Color := claSilver
  else
    g.Fill.Color := claWhite;

  g.FillRect(Rect, 0, 0, [], Translucent);
  g.DrawRect(Rect, 0, 0, [], Opaque);

  LLocation := aCell.BlockLocation;
  g.StrokeThickness := 3.0;
  g.Stroke.Color := claYellow;

  if TBlockPosition.Left in LLocation then
    g.DrawLine(Rect.TopLeft, PointF(Rect.Left, Rect.Bottom), Opaque);
  if (TBlockPosition.Right in LLocation) and (aCell.Col = ColCount) then
    g.DrawLine(PointF(Rect.Right, Rect.Top), PointF(Rect.Right, Rect.Bottom), Opaque);

  if TBlockPosition.Top in LLocation then
    g.DrawLine(Rect.TopLeft, PointF(Rect.Right, Rect.Top), Opaque);
  if (TBlockPosition.Bottom in LLocation) and (aCell.Row = RowCount) then
    g.DrawLine(PointF(Rect.Left, Rect.Bottom), PointF(Rect.Right, Rect.Bottom), Opaque);
end;

procedure TSudokuGraph.DrawCellData(g: TCanvas; aCell: ISudokuCell; Rect: TRectF; ASelected: Boolean);
var
  Candidates: TSudokuValues;
  N: TSudokuValue;
  R: TRectF;
  s: string;
  fs: single;

  procedure TextRect(R: TRectF; s: string; ha, va: TTextAlign);
  begin
    g.FillText(
      R,
      s,
      false, // WordWrap
      Opaque, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

begin
  Rect.Inflate(-1, -1);

  N := aCell.Value;
  if N > 0 then
  begin
    fs := Rect.Height * 75 / 100;
    g.Font.Size := fs;

    if aCell.IsValid then
    begin
      g.Stroke.Color  := claBlack;
      g.Fill.Color  := claBlack;
    end
    else
    begin
      g.Stroke.Color  := claGray;
      g.Fill.Color := claRed;
    end;

    s := GetSymbol(N);
    TextRect(Rect, s, TTextAlign.Center, TTextAlign.Center);
  end
  else
  begin
    g.Stroke.Color := claSilver;
    if ASelected then
      g.Fill.Color := claYellow
    else
      g.Fill.Color := claSilver;
    g.FillRect(Rect, 0, 0, NoCorners, Translucent);

    Candidates := aCell.Candidates;
    if Candidates <> [] then
    begin
      fs := Rect.Height * 0.2;
      g.Font.Size := fs;
      g.Stroke.Color := claBlue;
      g.Fill.Color := claBlue;

      R := Rect;
      R.Right := R.Left + Rect.Width / 3;
      R.Bottom:= R.Top + Rect.Height / 3;
      for N := 1 to High(N) do
      begin
        if N in Candidates then
        begin
          s := GetSymbol(N);
          TextRect(R, S, TTextAlign.Center, TTextAlign.Center);
          OffsetRect(R, R.Width, 0);
          if R.Right > Rect.Right then
            OffsetRect(R, -R.Width * 3, R.Height);
        end;
      end;
    end;
  end;
end;

procedure TSudokuGraph.DrawCell(g: TCanvas; ACell: ISudokuCell; ACol, ARow: Integer; Rect: TRectF);
var
  LSelected: Boolean;
begin
  LSelected := (ACol = Col) and (ARow = Row);
  DrawCellBorders(g, ACell, Rect, LSelected);
  DrawCellData(g, ACell, Rect, LSelected);
end;

procedure TSudokuGraph.DrawSudokuGrid(g: TCanvas);
var
  r, c: Integer;
  RF: TRectF;
  LSudokuCell: ISudokuCell;
begin
  if DataStorage = nil then
    Exit;

  for c := 0 to ColCount-1 do
  begin
    for r := 0 to RowCount-1 do
    begin
      LSudokuCell := DataStorage.Cell[c + 1, r + 1];
      RF.Left := DefaultColWidth * c;
      RF.Top := DefaultRowHeight * r;
      RF.Right := RF.Left + DefaultColWidth;
      RF.Bottom := RF.Top + DefaultRowHeight;
      DrawCell(g, LSudokuCell, c, r, RF);
    end;
  end;
end;

function TSudokuGraph.GetSymbol(aValue: TSudokuValue): string;
begin
  Result := string.Parse(aValue);
end;

end.

