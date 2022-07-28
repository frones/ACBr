unit ACBrJSON;

interface

uses
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}
    Jsons,
  {$EndIf}
  Classes,
  SysUtils;

type
  TACBrJSONArray = class;

  TACBrJSON = class
  public
    function ToJSON: string; virtual; abstract;
  end;

  TACBrJSONObject = class(TACBrJSON)
  private
    FJSON: TJsonObject;
    FContexts: TList;
    FOwnerJSON: Boolean;

    class function CreateJsonObject(const AJsonString: string): TJsonObject;

    function GetAsBoolean(const AName: string): Boolean;
    function GetAsCurrency(const AName: string): Currency;
    function GetAsFloat(const AName: string): Double;
    function GetAsInteger(const AName: string): Integer;
    function GetAsISODateTime(const AName: string): TDateTime;
    function GetAsString(const AName: string): string;
    function GetAsISOTime(const AName: string): TDateTime;
    function GetAsJSONArray(const AName: string): TACBrJSONArray;
    function GetAsJSONObject(const AName: string): TACBrJSONObject;
    function GetAsISODate(const AName: string): TDateTime;
    function GetAsSplitResult(const AName: string): TSplitResult;

  public
    function AddPair(const AName: string; const AValue: Boolean): TACBrJSONObject; overload;
    function AddPair(const AName, AValue: string): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: Integer): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: Double): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: array of string): TACBrJSONObject; overload;
    function AddPair(const AName: string; AValue: TACBrJSONArray): TACBrJSONObject; overload;
    function AddPair(const AName: string; AValue: TACBrJSONObject): TACBrJSONObject; overload;
    function AddPairISODateTime(const AName: string; const AValue: TDateTime): TACBrJSONObject; overload;
    function AddPairISOTime(const AName: string; const AValue: TDateTime): TACBrJSONObject; overload;
    function AddPairJSONObject(const AName: string; const AValue: string): TACBrJSONObject; overload;
    function AddPairJSONArray(const AName: string; const AValue: string): TACBrJSONObject; overload;

    function Value(const AName: string; var AValue: Boolean): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: Integer): TACBrJSONObject; overload;
    function ValueISODate(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function ValueISODateTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function ValueISOTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function Value(const AName: string; var AValue: Double): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: Currency): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: string): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: TSplitResult): TACBrJSONObject; overload;

    property OwnerJSON: Boolean read FOwnerJSON write FOwnerJSON;
    property AsBoolean[const AName: string]: Boolean read GetAsBoolean;
    property AsCurrency[const AName: string]: Currency read GetAsCurrency;
    property AsFloat[const AName: string]: Double read GetAsFloat;
    property AsInteger[const AName: string]: Integer read GetAsInteger;
    property AsISODateTime[const AName: string]: TDateTime read GetAsISODateTime;
    property AsISODate[const AName: string]: TDateTime read GetAsISODate;
    property AsISOTime[const AName: string]: TDateTime read GetAsISOTime;
    property AsString[const AName: string]: string read GetAsString;
    property AsSplit[const AName: string]: TSplitResult read GetAsSplitResult;
    property AsJSONObject[const AName: string]: TACBrJSONObject read GetAsJSONObject;
    property AsJSONArray[const AName: string]: TACBrJSONArray read GetAsJSONArray;

    function ToJSON: string; override;
    class function Parse(const AJSONString: string): TACBrJSONObject;

    constructor Create; overload;
    constructor Create(AJSONObject: TJsonObject); overload;
    destructor Destroy; override;
  end;

  TACBrJSONArray = class(TACBrJSON)
  private
    FJSON: TJsonArray;
    FContexts: TList;
    FOwnerJSON: Boolean;

    class function CreateJsonArray(const AJsonString: string): TJsonArray;
    function GetItems(const AIndex: Integer): string;
    function GetItemAsJSONObject(const AIndex: Integer): TACBrJSONObject;

  public
    property OwnerJSON: Boolean read FOwnerJSON write FOwnerJSON;
    property Items[const AIndex: Integer]: string read GetItems;
    property ItemAsJSONObject[const AIndex: Integer]: TACBrJSONObject read GetItemAsJSONObject;

    function AddElement(const AValue: string): TACBrJSONArray; overload;
    function AddElementJSONString(const AValue: string): TACBrJSONArray; overload;

    procedure Clear;
    function Count: Integer;
    function ToJSON: string; override;
    class function Parse(const AJSONString: string): TACBrJSONArray;

    constructor Create; overload;
    constructor Create(AJSONArray: TJsonArray); overload;
    destructor Destroy; override;
  end;

implementation

{ TACBrJSONObject }

constructor TACBrJSONObject.Create;
begin
  FJSON := TJsonObject.Create;
  FOwnerJSON := True;
  FContexts := TList.Create;
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Boolean): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.B[AName] := AValue;
  {$Else}
    FJson[AName].AsBoolean := AValue;
  {$EndIf}
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Double): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.F[AName] := AValue;
  {$Else}
    FJson[AName].AsNumber := AValue;
  {$EndIf}
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Integer): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.I[AName] := AValue;
  {$Else}
    FJson[AName].AsInteger := AValue;
  {$EndIf}
end;

function TACBrJSONObject.AddPair(const AName, AValue: string): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.S[AName] := AValue;
  {$Else}
    FJson[AName].AsString := AValue;
  {$EndIf}
end;

function TACBrJSONObject.AddPairISODateTime(const AName: string; const AValue: TDateTime): TACBrJSONObject;
var
  LValue: string;
begin
  Result := Self;
  LValue := '';
  if AValue > 0 then
    LValue := DateTimeToIso8601(AValue);

  AddPair(AName, LValue);
end;

function TACBrJSONObject.AddPairISOTime(const AName: string; const AValue: TDateTime): TACBrJSONObject;
var
  LValue: string;
begin
  Result := Self;
  LValue := '';
  if AValue > 0 then
    LValue := FormatDateTime('hh:mm:ss', AValue);

  AddPair(AName, LValue);
end;

function TACBrJSONObject.AddPairJSONArray(const AName, AValue: string): TACBrJSONObject;
var
  LJSONArray: TJsonArray;
begin
  Result := Self;
  LJSONArray := TJsonArray.Create;
  try
    LJSONArray.Parse(AValue);
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJSON.A[AName] := LJSONArray;
    {$ELSE}
    FJSON.Put(AName, AValue);
    {$ENDIF}
  finally
    LJSONArray.Free;
  end;
end;

function TACBrJSONObject.AddPairJSONObject(const AName: string; const AValue: string): TACBrJSONObject;
var
  LJSON: TJsonObject;
begin
  Result := Self;
  LJSON := CreateJsonObject(AValue);
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     FJson.O[AName] := LJSON.O[AName].Clone;
    {$Else}
    FJson[AName].AsObject := LJSON[AName].AsObject;
    {$EndIf}
  finally
    LJSON.Free;
  end;
end;

function TACBrJSONObject.GetAsBoolean(const AName: string): Boolean;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJSON.B[AName];
  {$Else}
    Result := FJSON[AName].AsBoolean;
  {$EndIf}
end;

function TACBrJSONObject.GetAsCurrency(const AName: string): Currency;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJson.D[AName];
  {$Else}
    Result := FJson[AName].AsNumber;
  {$EndIf}
end;

function TACBrJSONObject.GetAsFloat(const AName: string): Double;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJson.D[AName];
  {$Else}
    Result := FJson[AName].AsNumber;
  {$EndIf}
end;

function TACBrJSONObject.GetAsInteger(const AName: string): Integer;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJson.I[AName];
  {$Else}
    Result := FJson[AName].AsInteger;
  {$EndIf}
end;

function TACBrJSONObject.GetAsISODate(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LStrValue := FJson.S[AName];
  {$Else}
    LStrValue := FJson[AName].AsString;
  {$EndIf}
  if LStrValue <> '' then
    Result := EncodeDataHora(LStrValue, 'yyyy-MM-dd');
end;

function TACBrJSONObject.GetAsISODateTime(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LStrValue := FJson.S[AName];
  {$Else}
    LStrValue := FJson[AName].AsString;
  {$EndIf}
  if LStrValue <> '' then
    Result := Iso8601ToDateTime(LStrValue);
end;

function TACBrJSONObject.GetAsJSONArray(const AName: string): TACBrJSONArray;
var
  LJSON: TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LJSON := FJSON.A[AName];
  {$Else}
    LJSON := FJSON[AName].AsArray;
  {$EndIf}

  Result := TACBrJSONArray.Create(LJSON);
  FContexts.Add(Result);
end;

function TACBrJSONObject.GetAsJSONObject(const AName: string): TACBrJSONObject;
var
  LJSON: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LJSON := FJSON.O[AName];
  {$Else}
    LJSON := FJSON[AName].AsObject;
  {$EndIf}

  Result := TACBrJSONObject.Create(LJSON);
  FContexts.Add(Result);
end;

function TACBrJSONObject.GetAsSplitResult(const AName: string): TSplitResult;
var
  LJSONArray: TACBrJSONArray;
  I: Integer;
begin
  LJSONArray := GetAsJSONArray(AName);
  if Assigned(LJSONArray) then
  begin
    SetLength(Result, LJSONArray.Count);
    for I := 0 to Pred(LJSONArray.Count) do
      Result[I] := LJSONArray.Items[I];
  end;
end;

function TACBrJSONObject.GetAsString(const AName: string): string;
{$IfNDef USE_JSONDATAOBJECTS_UNIT}
var
  LValue: TJsonValue;
{$ENDIF}
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJson.S[AName]
  {$Else}
    LValue := FJson[AName];
    if Assigned(LValue) then
      Result := LValue.AsString;
  {$EndIf}
end;

function TACBrJSONObject.GetAsISOTime(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LStrValue := FJson.S[AName];
  {$Else}
    LStrValue := FJson[AName].AsString;
  {$EndIf}
  if LStrValue <> '' then
    Result := StringToDateTime(Copy(LStrValue, 1, 8), 'hh:mm:ss');
end;

class function TACBrJSONObject.Parse(const AJSONString: string): TACBrJSONObject;
var
  LJSON: TJsonObject;
begin
  LJSON := CreateJsonObject(AJSONString);
  try
    Result := TACBrJSONObject.Create(LJSON);
    Result.OwnerJSON := True;
  except
    LJSON.Free;
    raise;
  end;
end;

function TACBrJSONObject.ToJSON: string;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJSON.ToJSON();
  {$Else}
    Result := FJSON.Stringify;
  {$EndIf}
end;

constructor TACBrJSONObject.Create(AJSONObject: TJsonObject);
begin
  FOwnerJSON := False;
  FJSON := AJSONObject;
  FContexts := TList.Create;
end;

class function TACBrJSONObject.CreateJsonObject(const AJsonString: string): TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonSerializationConfig.NullConvertsToValueTypes := True;
    Result := TJsonObject.Parse(AJsonString) as TJsonObject;
  {$Else}
    Result := TJsonObject.Create;
    try
      Result.Parse(AJsonString);
    except
      Result.Free;
      raise;
    end;
  {$EndIf}
end;

destructor TACBrJSONObject.Destroy;
var
  I: Integer;
begin
  for I := Pred(FContexts.Count) downto 0 do
    TACBrJSONObject(FContexts.Items[I]).Free;
  FContexts.Free;
  if FOwnerJSON then
    FJSON.Free;
  inherited;
end;

function TACBrJSONObject.Value(const AName: string; var AValue: Boolean): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsBoolean(AName);
end;

function TACBrJSONObject.ValueISODate(const AName: string; var AValue: TDateTime): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsISODate(AName);
end;

function TACBrJSONObject.ValueISODateTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsISODateTime(AName);
end;

function TACBrJSONObject.ValueISOTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsISOTime(AName);
end;

function TACBrJSONObject.Value(const AName: string; var AValue: Double): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsFloat(AName);
end;

function TACBrJSONObject.Value(const AName: string; var AValue: Integer): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsInteger(AName);
end;

function TACBrJSONObject.Value(const AName: string; var AValue: string): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsString(AName);
end;

function TACBrJSONObject.Value(const AName: string; var AValue: Currency): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsCurrency(AName);
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: array of string): TACBrJSONObject;
var
  LStr: string;
  I: Integer;
  LJSONArray: TJsonArray;
begin
  Result := Self;
  LStr := '[';
  for I := 0 to Pred(Length(AValue)) do
  begin
    if I > 0 then
      LStr := LStr + ',';
    LStr := LStr + '"' + AValue[I] + '"';
  end;
  LStr := LStr + ']';

  LJSONArray := TACBrJSONArray.CreateJsonArray(LStr);
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.A[AName] := LJSONArray.Clone;
    {$Else}
    FJson[AName].AsArray := LJSONArray;
    {$EndIf}
  finally
    LJSONArray.Free;
  end;
end;

function TACBrJSONObject.Value(const AName: string; var AValue: TSplitResult): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsSplitResult(AName);
end;

function TACBrJSONObject.AddPair(const AName: string; AValue: TACBrJSONArray): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  FJSON.A[AName] := TACBrJSONArray.CreateJsonArray(AValue.ToJSON);
  {$ELSE}
  FJSON.Put(AName, AValue.FJSON);
  {$ENDIF}
  AValue.OwnerJSON := True;
  FContexts.Add(AValue);
end;

function TACBrJSONObject.AddPair(const AName: string; AValue: TACBrJSONObject): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  FJSON.O[AName] := TACBrJSONObject.CreateJsonObject(AValue.ToJSON);
  {$ELSE}
  FJSON.Put(AName, AValue.FJSON);
  {$ENDIF}
  AValue.OwnerJSON := True;
  FContexts.Add(AValue);
end;

{ TACBrJSONArray }

constructor TACBrJSONArray.Create;
begin
  FOwnerJSON := True;
  FJSON := TJsonArray.Create;
  FContexts := TList.Create;
end;

function TACBrJSONArray.AddElement(const AValue: string): TACBrJSONArray;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  FJSON.Add(AValue);
  {$Else}
  FJSON.Put(AValue);
  {$EndIf}
end;

function TACBrJSONArray.AddElementJSONString(const AValue: string): TACBrJSONArray;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJSON.AddObject(TACBrJSONObject.CreateJsonObject(AValue));
  {$Else}
    FJSON.Add.Parse(AValue);
  {$EndIf}
end;

procedure TACBrJSONArray.Clear;
begin
  FJSON.Clear;
end;

function TACBrJSONArray.Count: Integer;
begin
  Result := FJSON.Count;
end;

constructor TACBrJSONArray.Create(AJSONArray: TJsonArray);
begin
  FOwnerJSON := False;
  FJSON := AJSONArray;
  FContexts := TList.Create;
end;

class function TACBrJSONArray.CreateJsonArray(const AJsonString: string): TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonSerializationConfig.NullConvertsToValueTypes := True;
    Result := TJsonArray.Parse(AJsonString) as TJsonArray;
  {$Else}
    Result := TJsonArray.Create;
    try
      Result.Parse(AJSONString);
    except
      Result.Free;
      raise;
    end;
  {$EndIf}
end;

destructor TACBrJSONArray.Destroy;
var
  I: Integer;
begin
  for I := Pred(FContexts.Count) downto 0 do
    TACBrJSONObject(FContexts.Items[I]).Free;
  FContexts.Free;
  if FOwnerJSON then
    FJSON.Free;
  inherited;
end;

function TACBrJSONArray.GetItemAsJSONObject(const AIndex: Integer): TACBrJSONObject;
var
  {$IfNDef USE_JSONDATAOBJECTS_UNIT}
  LJSONStr: string;
  {$EndIf}
  LJSON: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  LJSON := FJSON.Items[AIndex].ObjectValue.Clone;
  {$ELSE}
  LJSONStr := TJsonObject(FJSON.Items[AIndex]).Stringify;
  LJSON := TJsonObject.Create;
  LJSON.Parse(LJSONStr);
  {$ENDIF}
  try
    Result := TACBrJSONObject.Create(LJSON);
    Result.OwnerJSON := True;
    FContexts.Add(Result);
  except
    LJSON.Free;
    raise;
  end;
end;

function TACBrJSONArray.GetItems(const AIndex: Integer): string;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := FJSON.Items[AIndex].Value;
  {$Else}
  Result := FJSON.Items[AIndex].AsString;
  {$EndIf}
end;

class function TACBrJSONArray.Parse(const AJSONString: string): TACBrJSONArray;
var
  LJSON: TJsonArray;
begin
  LJSON := CreateJsonArray(AJSONString);
  try
    Result := TACBrJSONArray.Create(LJSON);
    Result.OwnerJSON := True;
  except
    LJSON.Free;
    raise;
  end;
end;

function TACBrJSONArray.ToJSON: string;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := FJSON.ToJSON();
  {$Else}
    Result := FJSON.Stringify;
  {$EndIf}
end;

end.
