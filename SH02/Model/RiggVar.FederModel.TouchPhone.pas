﻿unit RiggVar.FederModel.TouchPhone;

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
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  RiggVar.FB.ActionConst,
  RiggVar.FederModel.TouchBase;

type
  TFederTouchPhone = class(TFederTouchBase)
  private
    procedure InitCornerMenu;
  protected
    procedure InitShapes;
    procedure SetActionMap(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; override;
    procedure InitActions(Layout: Integer); override;
    procedure UpdateColorScheme; override;
    procedure UpdateText; override;
  end;

implementation

uses
  RiggVar.App.Main;

{ TFederTouchPhone }

constructor TFederTouchPhone.Create(AOwner: TComponent);
begin
  inherited;
  ActionMap := 1;
  OpacityValue := 0.5;
  Main.ActionMapPhone.ActionProc := InitAction;
  Main.ActionMapPhone.ActionColorProc := InitActionWithColor;
end;

procedure TFederTouchPhone.Init;
begin
  if not InitOK then
  begin
    InitShapes;
    UpdateColorScheme;
    InitOK := True;
    UpdateShape;
  end;
end;

procedure TFederTouchPhone.InitShapes;
begin
  if not InitOK then
  begin
{$ifdef WantToolBtn}
    ToolBtn := TCircle.Create(OwnerComponent);
    ToolBtn.Position.X := MainVar.Raster;
    ToolBtn.Position.Y := MainVar.Raster;
    ToolBtn.Width := MainVar.Raster;
    ToolBtn.Height := MainVar.Raster;
    ToolBtn.Opacity := 0.1;
    ToolBtn.Fill.Color := MainVar.ColorScheme.claToolBtnFill;
    ToolBtn.OnClick := ToolBtnClick;
    ParentObject.AddObject(ToolBtn);
{$endif}

    InitCornerMenu;

    SL00.Shape.OnMouseDown := OnMouseDown;
    SR00.Shape.OnMouseDown := OnMouseDown;
    ST00.Shape.OnMouseDown := OnMouseDown;
    SB00.Shape.OnMouseDown := OnMouseDown;

    SL00.Shape.OnMouseMove := OnMouseMove;
    SR00.Shape.OnMouseMove := OnMouseMove;
    ST00.Shape.OnMouseMove := OnMouseMove;
    SB00.Shape.OnMouseMove := OnMouseMove;

    SL00.Shape.OnMouseUp := OnMouseUp;
    SR00.Shape.OnMouseUp := OnMouseUp;
    ST00.Shape.OnMouseUp := OnMouseUp;
    SB00.Shape.OnMouseUp := OnMouseUp;

    SL00.Shape.OnMouseLeave := OnMouseLeave;
    SR00.Shape.OnMouseLeave := OnMouseLeave;
    ST00.Shape.OnMouseLeave := OnMouseLeave;
    SB00.Shape.OnMouseLeave := OnMouseLeave;
  end;
end;

procedure TFederTouchPhone.UpdateColorScheme;
var
  b: TCornerBtn;
  tc1, tc2: TAlphaColor;
begin
{$ifdef WantToolBtn}
  ToolBtn.Fill.Color := MainVar.ColorScheme.claToolBtnFill;
{$endif}

  for b in CornerBtnList do
    b.Text.Color := MainVar.ColorScheme.claCornerBtnText;

  tc1 := MainVar.ColorScheme.claCornerScrollbar;
  tc2 := MainVar.ColorScheme.claCornerBtnText;

  ST00.Shape.Fill.Color := tc1;
  ST00.Text.Color := tc2;
  SB00.Shape.Fill.Color := tc1;
  SB00.Text.Color := tc2;
  SL00.Shape.Fill.Color := tc1;
  SL00.Text.Color := tc2;
  SR00.Shape.Fill.Color := tc1;
  SR00.Text.Color := tc2;
end;

procedure TFederTouchPhone.UpdateText;
begin
  {
    overwritten to do nothing
    NOT calling inherited here
    buttons 9 and 10 not used for pageing
    can use keyboard to switch between pages
  }
end;

procedure TFederTouchPhone.InitCornerMenu;
var
  cp: TCornerPos;
  cl: TAlphaColor;
  fa: TFederAction;
begin
  TCornerBtn.OffsetX := 0;
  TCornerBtn.OffsetY := 0;
  TCornerBtn.BtnWidth := MainVar.Raster;
  TCornerBtn.BtnHeight := MainVar.Raster;
  TCornerBtn.Circle := False;

  fa := faNoop;
  cl := claYellow;
  PageBtnP := CornerMenu.NewBtn(cpTR, 0, 0, cl, faActionPageP, 9);
  PageBtnM := CornerMenu.NewBtn(cpTL, 0, 0, cl, faActionPageM, 10);
  CornerBtnList.Add(PageBtnP);
  CornerBtnList.Add(PageBtnM);
  cl := claCornflowerBlue;
  CornerBtnList.Add(CornerMenu.NewBtn(cpBL, 0, 0, cl, fa, 7));
  CornerBtnList.Add(CornerMenu.NewBtn(cpBR, 0, 0, cl, fa, 8));

  cp := cpTL;
  cl := claWhite;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 1));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 2));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 3));

  cp := cpBR;
  cl := claWhite;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 6));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 5));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 4));

  cl := MainVar.ColorScheme.claCornerScrollbar;
  ST00 := CornerMenu.NewBtn(cpT, 0, 0, cl, faTouchBarTop);
  SR00 := CornerMenu.NewBtn(cpR, 0, 0, cl, faTouchBarRight);
  SB00 := CornerMenu.NewBtn(cpB, 0, 0, cl, faTouchBarBottom);
  SL00 := CornerMenu.NewBtn(cpL, 0, 0, cl, faTouchBarLeft);

  ST00.Text.Align := TAlignLayout.Client;
  ST00.Text.HitTest := False;
  ST00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SB00.Text.Align := TAlignLayout.Client;
  SB00.Text.HitTest := False;
  SB00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SL00.Text.Align := TAlignLayout.Client;
  SL00.Text.HitTest := False;
  SL00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SR00.Text.Align := TAlignLayout.Client;
  SR00.Text.HitTest := False;
  SR00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  ST00.Text.Text := '';
  SB00.Text.Text := '';
  SL00.Text.Text := '';
  SR00.Text.Text := '';

  InitActions(1);
end;

procedure TFederTouchPhone.InitActions(Layout: Integer);
begin
  Main.ActionMapPhone.InitActions(Layout);
end;

procedure TFederTouchPhone.SetActionMap(const Value: Integer);
begin
  inherited;
  MaxPageIndex := Main.ActionMapPhone.PageCount;
  EscapePageIndex := Main.ActionMapPhone.EscapeIndex;
end;

end.
