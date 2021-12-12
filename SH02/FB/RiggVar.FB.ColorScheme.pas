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

    claBackground3D: TAlphaColor;

    claLabelText: TAlphaColor;
    claSampleText: TAlphaColor;
    claOptionText: TAlphaColor;
    claToolBtnFill: TAlphaColor;
    claTouchBtnFill: TAlphaColor;
    claCornerScrollbar: TAlphaColor;
    claCornerBtnText: TAlphaColor;
    claEquationFill: TAlphaColor;
    claEquationText: TAlphaColor;
    claTouchbarText: TAlphaColor;

    constructor Create(cs: Integer);

    procedure Init(cs: Integer);
  end;

implementation

{ TColorScheme }

constructor TColorScheme.Create(cs: Integer);
begin
  claTouchbarText := claBlack;
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
      claBackground3D := claLavender;
      claLabelText := claGray;
      claSampleText := claGray;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claBlue;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
    2:
    begin
      claBackground3D := StringToAlphaColor('#FFBBBBBB');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claBlue;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
    3:
    begin
      claBackground3D := claCornflowerBlue;
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
    4:
    begin
      claBackground3D := StringToAlphaColor('#FF372E69');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claWhite;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    5:
    begin
      claBackground3D := StringToAlphaColor('#FF333333');
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    6:
    begin
      claBackground3D := claBlack;
      claLabelText := claWhite;
      claSampleText := claWhite;
      claToolBtnFill := claWhite;
      claTouchBtnFill := claWhite;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claWhite;
    end;
    7:
    begin
      claBackground3D := claNull;
      claLabelText := claBlack;
      claSampleText := claBlack;
      claToolBtnFill := claGray;
      claTouchBtnFill := claGray;
      claCornerScrollbar := claGray;
      claCornerBtnText:= claWhite;
      claEquationFill := claNull;
      claEquationText := claBlack;
    end;
  end;
  claOptionText := claSampleText;
end;

end.
