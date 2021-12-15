unit RiggVar.FB.ColorScheme;

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
  System.UITypes,
  System.UIConsts;

type
  TColorScheme = record
  public
    CurrentScheme: Integer;
    DefaultScheme: Integer;

    claBackground: TAlphaColor;
    claLabelText: TAlphaColor;
    claToolBtnFill: TAlphaColor;
    claTouchBtnFill: TAlphaColor;
    claCornerScrollbar: TAlphaColor;
    claCornerBtnText: TAlphaColor;

    constructor Create(cs: Integer);

    procedure Init(cs: Integer);
  end;

implementation

{ TColorScheme }

constructor TColorScheme.Create(cs: Integer);
begin
  DefaultScheme := cs;
  CurrentScheme := DefaultScheme;
  Init(CurrentScheme);
end;

procedure TColorScheme.Init(cs: Integer);
begin
  CurrentScheme := cs;
  case cs of
    1:
    begin
      claBackground := claLavender;
      claLabelText := claGray;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claBlue;
    end;
    2:
    begin
      claBackground := StringToAlphaColor('#FFBBBBBB');
      claLabelText := claWhite;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claBlue;
    end;
    3:
    begin
      claBackground := claCornflowerBlue;
      claLabelText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
    end;
    4:
    begin
      claBackground := StringToAlphaColor('#FF372E69');
      claLabelText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
    end;
    5:
    begin
      claBackground := StringToAlphaColor('#FF333333');
      claLabelText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
    6:
    begin
      claBackground := claBlack;
      claLabelText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
    7:
    begin
      claBackground := claNull;
      claLabelText := claBlack;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
    end;
  end;
end;

end.
