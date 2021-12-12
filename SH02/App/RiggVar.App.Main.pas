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
  CmdLineInt: Integer = 0;
  CmdLineVal: string = '';

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
    BatchStopRequested: Boolean;
    WantTextureRepeat: Boolean;
    TextureRepeat: Boolean;
    ShouldRecycleSocket: Boolean;
    AppTitle: string;

    ColorScheme: TColorScheme;

    Raster: Integer;
    WheelFrequency: single;
    ShiftState: TShiftState;

    ClientWidth: Integer;
    ClientHeight: Integer;

    class constructor Create;
    class destructor Destroy;
  end;

implementation

{ MainVar }

class constructor MainVar.Create;
begin
  Raster := 70;
  WheelFrequency := 1;
  ShiftState := [];

  ColorScheme := TColorScheme.Create(5);
end;

class destructor MainVar.Destroy;
begin
  inherited;
end;

end.
