unit RiggVar.FederModel.ActionMapTablet;

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
  System.UIConsts,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionMap;

type
  TActionMapTablet = class(TActionMap)
  private
    procedure InitDefault;
    procedure InitAC(cl: TCornerLocation; bi: Integer; fa: TFederAction; cla: TAlphaColor);
  protected
    procedure InitActionsSH(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

{

FrameLocations = absolute position:

[01][03][04][05][06]----[14][13][12][11][07]
[02]------------------------------------[08]
----------------------------------------[09]
----------------------------------------[10]
--------------------------------------------
[22]----------------------------------------
[21]----------------------------------------
[20]------------------------------------[24]
[15][16][17][18][19]----[28][27][26][25][23]

CornerLocations = relative positions:

[1][2][3][4][5]----[1][2][3][4][5]
[6]----------------------------[6]
-------------------------------[7]
-------------------------------[8]
----------------------------------
[8]-------------------------------
[7]-------------------------------
[6]----------------------------[6]
[1][2][3][4][5]----[1][2][3][4][5]

}

implementation

uses
  RiggVar.App.Main;

constructor TActionMapTablet.Create;
begin
  inherited;
  FPageCount := 4;
  FEscapeIndex := 4;
  TestName := 'Tablet Page';
end;

procedure TActionMapTablet.InitActions(Layout: Integer);
begin
  InitDefault;
  InitActionsSH(Layout);
end;

procedure TActionMapTablet.InitActionsSH(Layout: Integer);
var
  cl: TCornerLocation;
  cla: TAlphaColor;
begin
  cla := ClaWhite;
  case Layout of
    1:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faSelect1, claPlum);
      InitAC(cl, 3, faSelect2, claPlum);
      InitAC(cl, 4, faSelect3, claPlum);
      InitAC(cl, 5, faSelect4, claPlum);
      InitAC(cl, 6, faSelect0, claAquaMarine);

      cl := TopRight;
      InitAC(cl, 1, faSelect5, claPlum);
      InitAC(cl, 2, faSelect6, claPlum);
      InitAC(cl, 3, faSelect7, claPlum);
      InitAC(cl, 4, faSelect8, claPlum);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faSelect9, claPlum);
      InitAC(cl, 7, faClearStack, claAquamarine);
      InitAC(cl, 8, faUndo, claAquamarine);

      cl := BottomLeft;
      InitAC(cl, 1, faSelect10, claCornflowerblue);
      InitAC(cl, 2, faSelect11, claCornflowerblue);
      InitAC(cl, 3, faSelect12, claCornflowerblue);
      InitAC(cl, 4, faSelect13, claCornflowerblue);
      InitAC(cl, 5, faSelect14, claCornflowerblue);
      InitAC(cl, 6, faSelect15, claCornflowerblue);
      InitAC(cl, 7, faSelect16, claCornflowerblue);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faSetFocusMode, claYellow);
      InitAC(cl, 2, faSetValueMode, claYellow);
      InitAC(cl, 3, faSetCandidateMode, claYellow);
      InitAC(cl, 4, faUnsetCandidateMode, claYellow);
      InitAC(cl, 5, faToggleGosuMode, claYellow);
      InitAC(cl, 6, faToggleGosu, claCoral);
    end;

    2:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faSudoku09A, claPlum);
      InitAC(cl, 3, faSudoku09B, claPlum);
      InitAC(cl, 4, faNoop, claPlum);
      InitAC(cl, 5, faNoop, claPlum);
      InitAC(cl, 6, faNoop, cla);

      cl := TopRight;
      InitAC(cl, 1, faSudoku12A, claPlum);
      InitAC(cl, 2, faSudoku12B, claPlum);
      InitAC(cl, 3, faSudoku12C, claPlum);
      InitAC(cl, 4, faSudoku12D, claPlum);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faSudoku16A, claPlum);
      InitAC(cl, 2, faSudoku16B, claPlum);
      InitAC(cl, 3, faSudoku16C, claPlum);
      InitAC(cl, 4, faSudoku16D, claPlum);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;

    3:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);

      cl := TopRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faShowActions, claPlum);
      InitAC(cl, 2, faShowMemo, claPlum);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faCycleColorSchemeM, claCornflowerblue);
      InitAC(cl, 5, faCycleColorSchemeP, claCornflowerblue);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faSaveSudoku, claAquamarine);
      InitAC(cl, 2, faLoadSudoku, claAquamarine);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faActionPageE, claYellow);
      InitAC(cl, 6, faNoop, cla);
    end;

    4:
    begin
      cla := claWhite;
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faTL02, cla);
      InitAC(cl, 3, faTL03, cla);
      InitAC(cl, 4, faTL04, claRed);
      InitAC(cl, 5, faTL05, cla);
      InitAC(cl, 6, faTL06, cla);

      cl := TopRight;
      InitAC(cl, 1, faTR01, cla);
      InitAC(cl, 2, faTR02, cla);
      InitAC(cl, 3, faTR03, cla);
      InitAC(cl, 4, faTR04, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faTR06, cla);
      InitAC(cl, 7, faTR07, cla);
      InitAC(cl, 8, faTR08, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faBL01, cla);
      InitAC(cl, 2, faBL02, cla);
      InitAC(cl, 3, faBL03, cla);
      InitAC(cl, 4, faBL04, cla);
      InitAC(cl, 5, faBL05, cla);
      InitAC(cl, 6, faBL06, cla);
      InitAC(cl, 7, faBL07, cla);
      InitAC(cl, 8, faBL08, cla);

      cl := BottomRight;
      InitAC(cl, 1, faBR01, cla);
      InitAC(cl, 2, faBR02, cla);
      InitAC(cl, 3, faBR03, cla);
      InitAC(cl, 4, faBR04, cla);
      InitAC(cl, 5, faBR05, cla);
      InitAC(cl, 6, faBR06, cla);
    end;

  end;
end;

procedure TActionMapTablet.InitDefault;
begin
  IAC(1, faActionPageM, claYellow);
  IAC(7, faActionPageP, claYellow);
end;

procedure TActionMapTablet.InitAC(cl: TCornerLocation; bi: Integer; fa: TFederAction; cla: TAlphaColor);
var
  j: Integer; // FrameLocation
begin
  { First, translate from CornerLocation to FrameLocation }
  j := 0;
  case cl of
    TopLeft:
    begin
      case bi of
        1: j := 1;
        2: j := 3;
        3: j := 4;
        4: j := 5;
        5: j := 6;
        6: j := 2;
      end;
    end;

    TopRight:
    begin
      case bi of
        1: j := 14;
        2: j := 13;
        3: j := 12;
        4: j := 11;
        5: j := 7;
        6: j := 8;
        7: j := 9;
        8: j := 10;
      end;
    end;

    BottomRight:
    begin
      case bi of
        1: j := 28;
        2: j := 27;
        3: j := 26;
        4: j := 25;
        5: j := 23;
        6: j := 24;
      end;
    end;

    BottomLeft:
    begin
      case bi of
        1: j := 15;
        2: j := 16;
        3: j := 17;
        4: j := 18;
        5: j := 19;
        6: j := 20;
        7: j := 21;
        8: j := 22;
      end;
    end;
  end;
  { Init button with Action and Color }
  IAC(j, fa, cla);
end;

end.
