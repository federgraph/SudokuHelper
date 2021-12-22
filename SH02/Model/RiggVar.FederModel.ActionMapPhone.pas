unit RiggVar.FederModel.ActionMapPhone;

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
  TActionMapPhone = class(TActionMap)
  private
    cla: TAlphaColor;
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TActionMapPhone.Create;
begin
  inherited;
  FPageCount := 2;
  FEscapeIndex := 2;
  TestName := 'Phone Page';
end;

procedure TActionMapPhone.InitActions(Layout: Integer);
begin
{
[10]----[9]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]

[10][1][2][3]----[9]
--------------------
--------------------
--------------------
--------------------
[7]-----[4][5][6][8]
}


  cla := claWhite;

  case Layout of
    1:
    begin
      IAC(1, faSelect2, claPlum);
      IAC(2, faSelect3, claPlum);
      IAC(3, faSelect4, claPlum);
      IAC(4, faSelect6, claPlum);
      IAC(5, faSelect7, claPlum);
      IAC(6, faSelect8, claPlum);
      IAC(8, faSelect9, claPlum);
      IAC(7, faSelect0, claAquaMarine);
      IAC(9, faSelect5, claPlum);
      IAC(10, faSelect1, claPlum);
    end;

    2:
    begin
      cla := claWhite;
      IAC(1, faMB01, cla);
      IAC(2, faMB02, cla);
      IAC(3, faMB03, cla);

      IAC(4, faMB04, cla);
      IAC(5, faMB05, cla);
      IAC(6, faMB06, cla);

      cla := claCornflowerBlue;
      IAC(7, faMB07, cla);
      IAC(8, faMB08, cla);

      cla := claYellow;
      IAC(9, faNoop, claYellow);
      IAC(10, faNoop, claAquamarine);
    end;

    else
    begin
      { do nothing }
    end;
  end;
end;

end.

