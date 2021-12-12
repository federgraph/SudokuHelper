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
    ForceActionPageMP: Boolean;
    procedure InitDefault;
    procedure InitAC(cl: TCornerLocation; bi, fa: Integer; cla: TAlphaColor);
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
  FPageCount := 2;
  FEscapeIndex := FPageCount + 1;
  ForceActionPageMP := True;
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
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faSelect10, claCornflowerblue);
      InitAC(cl, 2, faSelect11, claCornflowerblue);
      InitAC(cl, 3, faSelect12, claCornflowerblue);
      InitAC(cl, 4, faSelect13, claCornflowerblue);
      InitAC(cl, 5, faSelect14, claCornflowerblue);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faSelect15, claCornflowerblue);
      InitAC(cl, 2, faSelect16, claCornflowerblue);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;

    2:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faSudoku9, claPlum);
      InitAC(cl, 3, faSudoku12, claPlum);
      InitAC(cl, 4, faSudoku16, claPlum);
      InitAC(cl, 5, faNoop, claPlum);
      InitAC(cl, 6, faNoop, cla);

      cl := TopRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, claWhite);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faCycleColorSchemeM, cla);
      InitAC(cl, 5, faCycleColorSchemeP, cla);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;
  end;
end;

procedure TActionMapTablet.InitDefault;
begin
  IAC(1, faActionPageM, claYellow);
  IAC(7, faActionPageP, claYellow);
end;

procedure TActionMapTablet.InitAC(cl: TCornerLocation; bi,
  fa: Integer; cla: TAlphaColor);
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
