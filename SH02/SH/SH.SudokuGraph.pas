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
    FZoom: single;
    FImage: TOriginalImage; // injected, not owned

    SavedMatrix: TMatrix;
    NewMatrix: TMatrix;
    TempMatrix: TMatrix;

    procedure SetImage(const Value: TOriginalImage);
    procedure BeginTransform(g: TCanvas);
    procedure EndTransform(g: TCanvas);

    function GetSymbol(aValue: TSudokuValue): string;
    procedure DrawToCanvas(g: TCanvas);
    procedure ClearBackground(g: TCanvas);
    procedure DrawSudokuGrid(g: TCanvas);
    procedure DrawCell(g: TCanvas; ACell: ISudokuCell; ACol, ARow: Integer; Rect: TRectF);
    procedure DrawCellBorders(g: TCanvas; aCell: ISudokuCell; var Rect: TRectF; ASelected: Boolean);
    procedure DrawCellData(g: TCanvas; aCell: ISudokuCell; Rect: TRectF; ASelected: Boolean);
  public
    DrawCounter: Integer;
    BackgroundColor: TAlphaColor;
    ImageOpacity: single;

    DataStorage: ISudokuData; // injected

    constructor Create;

    procedure Draw; override;
    procedure AddToDebugText(ML: TStrings);

    property Zoom: single read FZoom;
    property Image: TOriginalImage read FImage write SetImage;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TSudokuGraph.Create;
begin
  inherited;
  BackgroundColor := claAntiquewhite;
  ImageOpacity := 1.0;
end;

procedure TSudokuGraph.SetImage(const Value: TOriginalImage);
begin
  FImage := Value;
  FImage.Width := 16 * 64; // big enough to display largest sudoku
  FImage.Height := FImage.Width;
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

  TestSizeH := MainVar.ClientHeight - 160; // 160 = MainVar.Raster + Margin
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
  if not DrawNeeded then
    Exit;
  if Image = nil then
    Exit;

  DrawNeeded := False;
  DrawToCanvas(Image.Bitmap.Canvas);
  Image.Repaint;
  Main.FederText.SL00.Caption := IntToStr(DrawCounter);
end;

procedure TSudokuGraph.ClearBackground(g: TCanvas);
var
  R: TRectF;
begin
  if Image = nil then
  begin
    g.Clear(BackgroundColor);
  end
  else
  begin
    { clear to claRed to see real size of image,
    notice how it might overlap frame buttons on the bottom edge of window
    which is why HitTest on the image has been set to false
    and clicks are handle on the form instead of image }
    g.Clear(claNull);
    R := RectF(0, 0, Width, Height);
    g.Fill.Color := BackgroundColor;
    g.FillRect(R, 0, 0, [], ImageOpacity);
  end;
end;

procedure TSudokuGraph.DrawCellBorders(g: TCanvas; aCell: ISudokuCell; var Rect: TRectF; ASelected: Boolean);
var
  LLocation: TCellInBlockLocation;
begin
  { fill cell background }
  if ASelected then
  begin
    if aCell.EvenOnly then
      g.Fill.Color := claAqua
    else
      g.Fill.Color := claYellow;
  end
  else
  begin
    if aCell.EvenOnly then
      g.Fill.Color := claSilver
    else
      g.Fill.Color := claWhite;
  end;
  g.FillRect(Rect, 0, 0, NoCorners, Translucent);

  { draw cell border rectangle }
  g.Stroke.Color := claPlum;
  g.StrokeThickness := 1.0;
  g.DrawRect(Rect, 0, 0, [], Translucent);

  { draw block border lines }
  g.Stroke.Color := MainVar.ColorScheme.claBackground;
  g.StrokeThickness := 1.0;

  LLocation := aCell.BlockLocation;
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
      False, // WordWrap
      1.0, // Opacity
      [], // [TFillTextFlag.RightToLeft],
      ha,
      va);
  end;

begin
  Rect.Inflate(-1.5, -1.5);

  N := aCell.Value;
  if N > 0 then
  begin
    { draw normal cell text }
    fs := Rect.Height * 75 / 100;
    g.Font.Size := fs;

    if aCell.IsValid then
    begin
      g.Fill.Color  := claBlack;
    end
    else
    begin
      g.Fill.Color := claRed;
    end;

    s := GetSymbol(N);
    TextRect(Rect, s, TTextAlign.Center, TTextAlign.Center);
  end
  else
  begin
    { draw candidates text }
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
  g.StrokeCap := TStrokeCap.Flat;
  g.StrokeJoin := TStrokeJoin.Miter;

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

procedure TSudokuGraph.AddToDebugText(ML: TStrings);
begin
  { debug output may be shown in FormMemo }
  ML.Add('TSudokuGraph.SizeInfo:');
  ML.Add(Format('  Client.WH = (%d, %d)', [MainVar.ClientWidth, MainVar.ClientHeight]));
  ML.Add(Format('  Graph.WH  = (%d, %d)', [Width, Height]));
  ML.Add(Format('  Image.WH  = (%4.0f, %4.0f)', [Image.Width, Image.Height]));
  ML.Add(Format('  Bitmap.WH = (%d, %d)', [Image.Bitmap.Width, Image.Bitmap.Height]));

{
Client.WH = (1024, 900) // ClientWidth, ClientHeight, will change
Graph.WH  = (576, 576) // may change when form is resized
Image.WH  = (1024, 1024) // fixed value of 16 * 64; see SetImage
Bitmap.WH = (1024, 1024) // may change when form is dragged to other monitor
}

end;

end.

