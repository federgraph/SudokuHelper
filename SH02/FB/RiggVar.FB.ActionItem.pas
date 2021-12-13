unit RiggVar.FB.ActionItem;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.FB.ActionConst;

type
  TActionItem = class
  private
    FValue: TFederAction;
    procedure SetValue(const Value: TFederAction);
    procedure Init;
  public
    Name: string;
    Short: string;
    Long: string;
//    EncodedValue: Integer;
//    DecodedValue: Integer;
    constructor Create(fa: TFederAction);
    procedure WriteCode(ML: TStrings);
    procedure WriteAll(ML: TStrings);
    property Value: TFederAction read FValue write SetValue;
//    property Def: Integer read EncodedValue;
  end;

  TActionBuilder = class
  public

  end;

implementation

uses
//  RiggVar.FB.ActionEncode,
//  RiggVar.FB.ActionDecode,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong,
  RiggVar.FB.ActionName;

{ TActionItem }

constructor TActionItem.Create(fa: TFederAction);
begin
  FValue := fa;
end;

procedure TActionItem.Init;
begin
  Short := GetFederActionShort(FValue);
  Long := GetFederActionLong(FValue);
  Name := GetFederActionName(FValue);
//  EncodedValue := GetFederActionEncodedValue(FValue);
//  DecodedValue := GetFederActionDecodedValue(FValue);
end;

procedure TActionItem.SetValue(const Value: TFederAction);
begin
  FValue := Value;
  Init;
end;

procedure TActionItem.WriteCode(ML: TStrings);
begin
  ML.Add(Format('ai := TActionItem.Create(%s);', [Name]));
  ML.Add(Format('ai.Name := ''%s'';', [Name]));
  ML.Add(Format('ai.Short := ''%s'';', [Short]));
  ML.Add(Format('ai.Long := ''%s'';', [Long]));
//  ML.Add(Format('ai.EncodedValue := %d;', [EncodedValue]));
//  ML.Add(Format('ai.DecodedValue := %d;', [DecodedValue]));
end;

procedure TActionItem.WriteAll(ML: TStrings);
var
  i: Integer;
begin
  for i := 0 to faMax - 1 do
  begin
    Value := i;
    WriteCode(ML);
    ML.Add('Add(ai);');
    ML.Add('');
  end;
end;

end.
