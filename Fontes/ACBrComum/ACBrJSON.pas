{$I ACBr.inc}

unit ACBrJSON;

interface

uses
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}{$IfDef FPC}
    fpjson,
  {$Else}
    Jsons,
  {$EndIf}{$EndIf}
  Classes,
  SysUtils;

type
  TACBrJSONValue = {$IfDef USE_JSONDATAOBJECTS_UNIT} TJsonDataValueHelper;
                   {$Else}{$IfDef FPC} TJSONData;
                   {$Else} TJsonValue;{$EndIf}{$EndIf}
  TACBrJSONArray = class;

  TACBrJSON = class
  public
    function ToJSON: string; virtual; abstract;
  end;

  { TACBrJSONObject }

  TACBrJSONObject = class(TACBrJSON)
  private
    FJSON: TJsonObject;
    FContexts: TList;
    FOwnerJSON: Boolean;

    class function CreateJsonObject(const AJsonString: string): TJsonObject;

    function GetAsValue(const AName: string): TACBrJSONValue;
    function GetAsBoolean(const AName: string): Boolean;
    function GetAsCurrency(const AName: string): Currency;
    function GetAsFloat(const AName: string): Double;
    function GetAsInteger(const AName: string): Integer;
    function GetAsInt64(const AName: string): Int64;
    function GetAsISODateTime(const AName: string): TDateTime;
    function GetAsString(const AName: string): string;
    function GetAsISOTime(const AName: string): TDateTime;
    function GetAsJSONArray(const AName: string): TACBrJSONArray;
    function GetAsJSONObject(const AName: string): TACBrJSONObject;
    function GetAsISODate(const AName: string): TDateTime;
    function GetAsSplitResult(const AName: string): TSplitResult;
    function IsNull(const AName: string): Boolean; overload;
    function IsNull(const AValue: TACBrJSONValue): Boolean; overload;
    function GetAsDateTimeBr(const AName: string): TDateTime;
  public
    function AddPair(const AName: string; const AValue: Boolean): TACBrJSONObject; overload;
    function AddPair(const AName, AValue: string; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: Integer; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: Int64; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: Double; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPair(const AName: string; const AValue: array of string): TACBrJSONObject; overload;
    function AddPair(const AName: string; AValue: TACBrJSONArray; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPair(const AName: string; AValue: TACBrJSONObject): TACBrJSONObject; overload;
    function AddPairISODateTime(const AName: string; const AValue: TDateTime; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPairISODate(const AName: string; const AValue: TDateTime; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPairISOTime(const AName: string; const AValue: TDateTime; AddEmpty: Boolean = True): TACBrJSONObject; overload;
    function AddPairJSONObject(const AName: string; const AValue: string): TACBrJSONObject; overload;
    function AddPairJSONArray(const AName: string; const AValue: string): TACBrJSONObject; overload;

    function Value(const AName: string; var AValue: Boolean): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: Integer): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: Int64): TACBrJSONObject; overload;
    function ValueISODate(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function ValueISODateTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function ValueISOTime(const AName: string; var AValue: TDateTime): TACBrJSONObject;
    function Value(const AName: string; var AValue: Double): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: Currency): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: string): TACBrJSONObject; overload;
    function Value(const AName: string; var AValue: TSplitResult): TACBrJSONObject; overload;

    function IsJSONArray(const AName: string): Boolean;

    property OwnerJSON: Boolean read FOwnerJSON write FOwnerJSON;
    property AsBoolean[const AName: string]: Boolean read GetAsBoolean;
    property AsCurrency[const AName: string]: Currency read GetAsCurrency;
    property AsFloat[const AName: string]: Double read GetAsFloat;
    property AsInteger[const AName: string]: Integer read GetAsInteger;
    property AsInt64[const AName: string]: Int64 read GetAsInt64;
    property AsISODateTime[const AName: string]: TDateTime read GetAsISODateTime;
    property AsISODate[const AName: string]: TDateTime read GetAsISODate;
    property AsDateTimeBr[const AName: string]: TDateTime read GetAsDateTimeBr;
    property AsISOTime[const AName: string]: TDateTime read GetAsISOTime;
    property AsString[const AName: string]: string read GetAsString;
    property AsSplit[const AName: string]: TSplitResult read GetAsSplitResult;
    property AsJSONObject[const AName: string]: TACBrJSONObject read GetAsJSONObject;
    property AsJSONArray[const AName: string]: TACBrJSONArray read GetAsJSONArray;
    property AsValue[const AName: string]: TACBrJSONValue read GetAsValue;

    function ToJSON: string; override;
    class function Parse(const AJSONString: string): TACBrJSONObject;

    constructor Create; overload;
    constructor Create(AJSONObject: TJsonObject); overload;
    destructor Destroy; override;
  end;

  { TACBrJSONArray }

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
    function AddElementJSON(AValue: TACBrJSONObject): TACBrJSONArray; overload;
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
  {$Else}{$IfDef FPC}
    FJSON.Booleans[AName] := AValue;
  {$Else}
    FJson[AName].AsBoolean := AValue;
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Double;
  AddEmpty: Boolean): TACBrJSONObject;
begin
  Result := Self;

  if (AValue = 0) and (not AddEmpty) then
    Exit;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.F[AName] := AValue;
  {$Else}{$IfDef FPC}
    FJSON.Floats[AName] := AValue;
  {$Else}
    FJson[AName].AsNumber := AValue;
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Integer;
  AddEmpty: Boolean): TACBrJSONObject;
begin
  Result := Self;

  if (AValue = 0) and (not AddEmpty) then
    Exit;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.I[AName] := AValue;
  {$Else}{$IfDef FPC}
    FJSON.Integers[AName] := AValue;
  {$Else}
    FJson[AName].AsInteger := AValue;
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.AddPair(const AName: string; const AValue: Int64;
  AddEmpty: Boolean): TACBrJSONObject;
begin
  Result := Self;

  if (AValue = 0) and (not AddEmpty) then
    Exit;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.L[AName] := AValue;
  {$Else}{$IfDef FPC}
    FJSON.Int64s[AName] := AValue;
  {$Else}
    FJson[AName].AsNumber := AValue;
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.AddPair(const AName, AValue: string; AddEmpty: Boolean = True): TACBrJSONObject;
begin
  Result := Self;

  if EstaVazio(AValue) and (not AddEmpty) then
    Exit;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJson.S[AName] := AValue;
  {$Else}{$IfDef FPC}
    FJSON.Strings[AName] := AValue;
  {$Else}
    FJson[AName].AsString := AValue;
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.AddPairISODateTime(const AName: string;
  const AValue: TDateTime; AddEmpty: Boolean): TACBrJSONObject;
var
  LValue: string;
begin
  Result := Self;
  LValue := '';
  if AValue > 0 then
    LValue := DateTimeToIso8601(AValue);

  AddPair(AName, LValue, AddEmpty);
end;

function TACBrJSONObject.AddPairISODate(const AName: string;
  const AValue: TDateTime; AddEmpty: Boolean): TACBrJSONObject;
var
  LValue: string;
begin
  Result := Self;
  LValue := EmptyStr;
  if (AValue > 0) then
    LValue := FormatDateTime('yyyy-MM-dd', AValue);

  AddPair(AName, LValue, AddEmpty);
end;

function TACBrJSONObject.AddPairISOTime(const AName: string;
  const AValue: TDateTime; AddEmpty: Boolean): TACBrJSONObject;
var
  LValue: string;
begin
  Result := Self;
  LValue := '';
  if AValue > 0 then
    LValue := FormatDateTime('hh:mm:ss', AValue);

  AddPair(AName, LValue, AddEmpty);
end;

function TACBrJSONObject.AddPairJSONArray(const AName: string;
  const AValue: string): TACBrJSONObject;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}var LJSONArray: TJsonArray;
  {$Else}{$IfDef FPC}var LJSONArray: TJsonArray;{$ENDIF}{$EndIf}
begin
  Result := Self;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  LJSONArray := TJsonArray.Create;
  try
    LJSONArray.Parse(AValue);
    FJSON.A[AName] := LJSONArray;
  finally
    LJSONArray.Free;
  end;
  {$Else}{$IfDef FPC}
  LJSONArray := GetJSON(AValue) as TJSONArray;
  try
    FJSON.Arrays[AName] := LJSONArray;
  finally
    LJSONArray.Free;
  end;
  {$ELSE}
  FJSON.Put(AName, AValue);
  {$ENDIF}{$EndIf}
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
    {$Else}{$IfDef FPC}
    if NaoEstaVazio(AValue) then
      FJSON.Objects[AName] := LJSON.Objects[AName].Clone as TJSONObject
    else if (FJSON.IndexOfName(AName) < 0) then
      FJSON.Add(AName, TJSONObject(LJSON.Clone));
    {$Else}
    FJson[AName].AsObject := LJSON[AName].AsObject;
    {$EndIf}{$EndIf}
  finally
    LJSON.Free;
  end;
end;

function TACBrJSONObject.GetAsBoolean(const AName: string): Boolean;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := False;
    Exit;
  end;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    Result := LValue.BoolValue
  {$Else}
    Result := LValue.AsBoolean;
  {$EndIf}
end;

function TACBrJSONObject.GetAsCurrency(const AName: string): Currency;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := 0;
    Exit;
  end;

{$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := LValue.FloatValue;
{$Else}{$IfDef FPC}
  Result := LValue.AsFloat;
{$Else}
  Result := LValue.AsNumber;
{$EndIf}{$EndIf}
end;

function TACBrJSONObject.GetAsFloat(const AName: string): Double;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := 0;
    Exit;
  end;

{$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := LValue.FloatValue;
{$Else}{$IfDef FPC}
  Result := LValue.AsFloat;
{$Else}
  Result := LValue.AsNumber;
{$EndIf}{$EndIf}
end;

function TACBrJSONObject.GetAsInteger(const AName: string): Integer;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := 0;
    Exit;
  end;

{$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := LValue.IntValue;
{$Else}
  Result := LValue.AsInteger;
{$EndIf}
end;

function TACBrJSONObject.GetAsInt64(const AName: string): Int64;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := 0;
    Exit;
  end;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := LValue.LongValue;
  {$Else}{$IfDef FPC}
  Result := LValue.AsInt64;
  {$Else}
  Result := Trunc(LValue.AsNumber);
  {$EndIf}{$EndIf}
end;

function TACBrJSONObject.GetAsISODate(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  LStrValue := GetAsString(AName);
  if LStrValue <> '' then
    Result := EncodeDataHora(LStrValue, 'yyyy-MM-dd');
end;

function TACBrJSONObject.GetAsDateTimeBr(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  LStrValue := GetAsString(AName);
  if LStrValue <> '' then
    Result := EncodeDataHora(LStrValue, 'DD-MM-YYYY');
end;

function TACBrJSONObject.GetAsISODateTime(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  LStrValue := GetAsString(AName);
  if LStrValue <> '' then
    Result := Iso8601ToDateTime(LStrValue);
end;

function TACBrJSONObject.GetAsJSONArray(const AName: string): TACBrJSONArray;
var
  LJSON: TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    LJSON := FJSON.A[AName];
  {$Else}{$IfDef FPC}
    LJSON := Nil;
    LJSON := FJSON.Get(AName, LJSON);
  {$Else}
    LJSON := FJSON[AName].AsArray;
  {$EndIf}{$EndIf}

  Result := TACBrJSONArray.Create(LJSON);
  FContexts.Add(Result);
end;

function TACBrJSONObject.GetAsJSONObject(const AName: string): TACBrJSONObject;
var
  LJSON: TJsonObject;
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := Nil;
    Exit;
  end;

{$IfDef USE_JSONDATAOBJECTS_UNIT}
  LJSON := FJSON.O[AName];
{$Else}{$IfDef FPC}
  LJSON := Nil;
  LJSON := FJSON.Get(AName, LJSON);
{$Else}
  LJSON := LValue.AsObject;
{$EndIf}{$EndIf}

  Result := TACBrJSONObject.Create(LJSON);
  FContexts.Add(Result);
end;

function TACBrJSONObject.GetAsSplitResult(const AName: string): TSplitResult;
var
  LJSONArray: TACBrJSONArray;
  I: Integer;
begin
  if IsNull(AName) then
    Exit;
  LJSONArray := GetAsJSONArray(AName);
  if Assigned(LJSONArray) then
  begin
    {$IfDef FPC}Result := Nil;{$EndIf}
    SetLength(Result, LJSONArray.Count);
    for I := 0 to Pred(LJSONArray.Count) do
      Result[I] := LJSONArray.Items[I];
  end;
end;

function TACBrJSONObject.GetAsString(const AName: string): string;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  if IsNull(LValue) then
  begin
    Result := EmptyStr;
    Exit;
  end;

{$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := LValue.Value;
{$Else}
  Result := LValue.AsString;
{$EndIf}
end;

function TACBrJSONObject.GetAsValue(const AName: string): TACBrJSONValue;
var
  LIndex: Integer;
begin
  Result := nil;
{$IfDef USE_JSONDATAOBJECTS_UNIT}
  LIndex := FJSON.IndexOf(AName);
  if LIndex >= 0 then
    Result := FJSON.Values[AName];
{$Else}{$IfDef FPC}
  LIndex := FJSON.IndexOfName(AName);
  if (LIndex >= 0) then
    Result := FJSON.Elements[AName];
{$Else}
  LIndex := FJSON.Find(AName);
  if LIndex >= 0 then
    Result := FJSON.Items[LIndex].Value;
{$EndIf}{$EndIf}
end;

function TACBrJSONObject.IsJSONArray(const AName: string): Boolean;
var
  JsonVal: TACBrJSONValue;
begin
  Result := False;

  JsonVal := AsValue[AName];
  if {$IfNDef USE_JSONDATAOBJECTS_UNIT}Assigned(JsonVal) and {$EndIf} (not JsonVal.IsNull) then
  begin
    {$IFDEF USE_JSONDATAOBJECTS_UNIT}
      if JsonVal.Typ = jdtArray then
        Result := True;
    {$ELSE}
      {$IFDEF FPC}
        if JsonVal.JSONType = jtArray then
          Result := True;
      {$ELSE}
        if JSonVal.ValueType = jvArray then
          Result := True;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TACBrJSONObject.IsNull(const AValue: TACBrJSONValue): Boolean;
begin
{$IfDef USE_JSONDATAOBJECTS_UNIT}
  Result := AValue.IsNull;
{$Else}
  Result := (not Assigned(AValue)) or (AValue.IsNull);
{$EndIf}
end;

function TACBrJSONObject.IsNull(const AName: string): Boolean;
var
  LValue: TACBrJSONValue;
begin
  LValue := GetAsValue(AName);
  Result := IsNull(LValue);
end;

function TACBrJSONObject.GetAsISOTime(const AName: string): TDateTime;
var
  LStrValue: string;
begin
  Result := 0;
  LStrValue := GetAsString(AName);
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
  {$Else}{$IfDef FPC}
    Result := FJSON.AsJSON;
  {$Else}
    Result := FJSON.Stringify;
  {$EndIf}{$EndIf}
end;

constructor TACBrJSONObject.Create(AJSONObject: TJsonObject);
begin
  FOwnerJSON := False;
  FJSON := AJSONObject;
  FContexts := TList.Create;
end;

class function TACBrJSONObject.CreateJsonObject(const AJsonString: string): TJsonObject;
begin
  Result := nil;
  try
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonSerializationConfig.NullConvertsToValueTypes := True;
    if NaoEstaVazio(AJsonString) then
      Result := TJsonObject.Parse(AJsonString) as TJsonObject
    else
      Result := TJsonObject.Create;
  {$Else}{$IfDef FPC}
    Result := GetJSON(AJsonString) as TJSONObject;
    if (not Assigned(Result)) then
      Result := TJSONObject.Create;
  {$Else}
    Result := TJsonObject.Create;
    if NaoEstaVazio(AJsonString) then
      Result.Parse(AJsonString);
  {$EndIf}{$EndIf}
  except
    if Assigned(Result) then
      Result.Free;
    raise;
  end;
end;

destructor TACBrJSONObject.Destroy;
var
  I: Integer;
begin
  for I := Pred(FContexts.Count) downto 0 do
    TACBrJSONObject(FContexts.Items[I]).Free;
  FContexts.Free;
  if FOwnerJSON and Assigned(FJSON) then
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

function TACBrJSONObject.Value(const AName: string; var AValue: Int64): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsInt64(AName);
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
    {$Else}{$IfDef FPC}
    FJSON.Arrays[AName] := LJSONArray.Clone as TJSONArray;
    {$Else}
    FJson[AName].AsArray := LJSONArray;
    {$EndIf}{$EndIf}
  finally
    LJSONArray.Free;
  end;
end;

function TACBrJSONObject.Value(const AName: string; var AValue: TSplitResult): TACBrJSONObject;
begin
  Result := Self;
  AValue := GetAsSplitResult(AName);
end;

function TACBrJSONObject.AddPair(const AName: string; AValue: TACBrJSONArray;
  AddEmpty: Boolean): TACBrJSONObject;
begin
  Result := Self;

  if (AValue.Count = 0) and (not AddEmpty) then
    Exit;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  FJSON.A[AName] := TACBrJSONArray.CreateJsonArray(AValue.ToJSON);
  {$Else}{$IfDef FPC}
  FJSON.Arrays[AName] := TACBrJSONArray.CreateJsonArray(AValue.ToJSON);
  {$ELSE}
  FJSON.Put(AName, AValue.FJSON);
  {$ENDIF}{$EndIf}
  AValue.OwnerJSON := True;
  FContexts.Add(AValue);
end;

function TACBrJSONObject.AddPair(const AName: string; AValue: TACBrJSONObject): TACBrJSONObject;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  FJSON.O[AName] := TACBrJSONObject.CreateJsonObject(AValue.ToJSON);
  {$Else}{$IfDef FPC}
  FJSON.Objects[AName] := TACBrJSONObject.CreateJsonObject(AValue.ToJSON);
  {$ELSE}
  FJSON.Put(AName, AValue.FJSON);
  {$ENDIF}{$EndIf}
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
  {$Else}{$IfDef FPC}
  FJSON.Add(AValue);
  {$Else}
  FJSON.Put(AValue);
  {$EndIf}{$EndIf}
end;

function TACBrJSONArray.AddElementJSON(AValue: TACBrJSONObject): TACBrJSONArray;
begin
  Result := Self;
  AddElementJSONString(AValue.ToJSON);
  FContexts.Add(AValue);
end;

function TACBrJSONArray.AddElementJSONString(const AValue: string): TACBrJSONArray;
begin
  Result := Self;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    FJSON.AddObject(TACBrJSONObject.CreateJsonObject(AValue));
  {$Else}{$IfDef FPC}
    FJSON.Add(TACBrJSONObject.CreateJsonObject(AValue));
  {$Else}
    FJSON.Add.Parse(AValue);
  {$EndIf}{$EndIf}
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
  if (not Assigned(AJSONArray)) then
  begin
    Create;
    Exit;
  end;

  FOwnerJSON := False;
  FJSON := AJSONArray;
  FContexts := TList.Create;
end;

class function TACBrJSONArray.CreateJsonArray(const AJsonString: string): TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  JsonSerializationConfig.NullConvertsToValueTypes := True;
  Result := TJsonArray.Parse(AJsonString) as TJsonArray;
  {$Else}{$IfDef FPC}
  Result := GetJSON(AJsonString) as TJSONArray;
  {$Else}
  Result := TJsonArray.Create;
  try
    Result.Parse(AJSONString);
  except
    Result.Free;
    raise;
  end;
  {$EndIf}{$EndIf}
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
  {$IfNDef USE_JSONDATAOBJECTS_UNIT}{$IfNDef FPC}
  LJSONStr: string;
  {$EndIf}{$EndIf}
  LJSON: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  LJSON := FJSON.Items[AIndex].ObjectValue.Clone;
  {$Else}{$IfDef FPC}
  LJSON := GetJSON(FJSON.Items[AIndex].AsJSON) as TJSONObject;
  {$ELSE}
  LJSONStr := TJsonObject(FJSON.Items[AIndex]).Stringify;
  LJSON := TJsonObject.Create;
  LJSON.Parse(LJSONStr);
  {$ENDIF}{$EndIf}
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
  {$Else}{$IfDef FPC}
  Result := FJSON.Items[AIndex].Value;
  {$Else}
  Result := FJSON.Items[AIndex].AsString;
  {$EndIf}{$EndIf}
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
  {$Else}{$IfDef FPC}
    Result := FJSON.AsJSON;
  {$Else}
    Result := FJSON.Stringify;
  {$EndIf}{$EndIf}
end;

end.
