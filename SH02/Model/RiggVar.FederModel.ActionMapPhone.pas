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

{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]

[9][1][2][3]----[10]
--------------------
--------------------
--------------------
--------------------
[7]------[4][5][6][8]

}

implementation

uses
  RiggVar.App.Main;

constructor TActionMapPhone.Create;
begin
  inherited;
  FPageCount := 1;
  FEscapeIndex := FPageCount + 1;
  TestName := 'Phone Page';
end;

procedure TActionMapPhone.InitActions(Layout: Integer);
begin
{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]
}

  cla := claWhite;

  case Layout of
    1:
    begin
      IAC(9, faSelect1, claPlum);
      IAC(1, faSelect2, claPlum);
      IAC(2, faSelect3, claPlum);
      IAC(3, faSelect4, claPlum);

      IAC(7, faSelect0, claAquaMarine);

      IAC(10, faSelect5, claPlum);

      IAC(4, faSelect6, claPlum);
      IAC(5, faSelect7, claPlum);
      IAC(6, faSelect8, claPlum);
      IAC(8, faSelect9, claPlum);

    end;

    else
    begin
      { do nothing }
    end;
  end;
end;

end.

