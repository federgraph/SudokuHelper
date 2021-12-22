unit RiggVar.App.Main;

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
  RiggVar.FB.ColorScheme,
  RiggVar.App.Main0;

type
  TMain = TMain0;

var
  Main: TMain;

type
  MainConst = class
  public const
    TrackbarFrequency = 0.1;
    ColorSchemeCount = 5;
    DefaultBtnFontSize = 24;
  end;

  MainVar = class
  class var
    AppIsClosing: Boolean;
    AppTitle: string;

    ColorScheme: TColorScheme;

    Raster: Integer;
    ShiftState: TShiftState;

    ClientWidth: Integer;
    ClientHeight: Integer;

    class constructor Create;
  end;

implementation

{ MainVar }

class constructor MainVar.Create;
begin
  Raster := 70;
  ShiftState := [];

  ColorScheme := TColorScheme.Create(5);
end;

end.
