{$I ACBr.inc}

unit JsonsUtilsEx;

interface


{$DEFINE LINEBREAKJSONFORMAT} //Desactivate for a non "minimal better human-readable format".

uses SysUtils;

function FixedFloatToStr(const Value: Extended): string;
function FixedTryStrToFloat(const S: string; out Value: Extended): Boolean;
function FixedStrToFloat(const S: string): Extended;

{$IFDEF DELPHIXE6_UP}
Function __ObjectToJson(aObject : TObject) : String;
Procedure __jsonToObject(Const aJSONString : String; Var aObject : TObject);
{$ENDIF}

Type
  TObjectDynArray = array of TObject;
  TStringDynArray = array of string;
  TIntegerDynArray = array of Integer;

Const
  GLB_JSON_STD_DECIMALSEPARATOR = '.';
Var
  JsonsUtils_GLB_DECIMALSEPARATOR : Char;

implementation

Uses TypInfo,
     DateUtils,
     Jsons;

Type
  PPPTypeInfo = ^PPTypeInfo;


//JSON date base conversion utility : taken "as is" from but quite incomplete. Will be replaced. TODO.

function ZeroFillStr(Number, Size : integer) : String;
begin
  Result := IntToStr(Number);
  while length(Result) < Size do
    Result := '0'+Result;
end;

function JSONDateToString(aDate : TDateTime) : String;
begin
  Result := '"'+ZeroFillStr(YearOf(aDate),4)+'-'+
            ZeroFillStr(MonthOf(aDate),2)+'-'+
            ZeroFillStr(DayOf(aDate),2)+'T'+
            ZeroFillStr(HourOf(aDate),2)+':'+
            ZeroFillStr(MinuteOf(aDate),2)+':'+
            ZeroFillStr(SecondOf(aDate),2)+'.'+
            ZeroFillStr(SecondOf(aDate),3)+'Z"';
end;

function JSONStringToDate(aDate : String) : TDateTime;
begin
  Result :=
    EncodeDateTime(
      StrToInt(Copy(aDate,1,4)),
      StrToInt(Copy(aDate,6,2)),
      StrToInt(Copy(aDate,9,2)),
      StrToInt(Copy(aDate,12,2)),
      StrToInt(Copy(aDate,15,2)),
      StrToInt(Copy(aDate,18,2)),
      StrToInt(Copy(aDate,21,3)));
end;

function JSONStringIsCompatibleDate(aJSONDate : String) : boolean;
var ldummy: integer;
    lval, lnum : Boolean;
begin
  lval := TryStrToInt(Copy(aJSONDate,1,4),ldummy) and  TryStrToInt(Copy(aJSONDate,6,2),ldummy) and
          TryStrToInt(Copy(aJSONDate,9,2),ldummy) and  TryStrToInt(Copy(aJSONDate,12,2),ldummy) and
          TryStrToInt(Copy(aJSONDate,15,2),ldummy) and TryStrToInt(Copy(aJSONDate,18,2),ldummy) and
          TryStrToInt(Copy(aJSONDate,21,3),ldummy);

  lnum := (Length(aJSONDate)=24) and
            (aJSONDate[5] = '-') and
            (aJSONDate[8] = '-') and
            (aJSONDate[11] = 'T') and
            (aJSONDate[14] = ':') and
            (aJSONDate[17] = ':') and
            (aJSONDate[20] = '.') and
            (aJSONDate[24] = 'Z');

  Result := lval and lNum;
end;


{**
 * Fixed FloatToStr to convert DecimalSeparator to dot (.) decimal separator, FloatToStr returns
 * DecimalSeparator as decimal separator, but JSON uses dot (.) as decimal separator.
 *}
function GetDecimalSeparator : Char;
  {$IFDEF FPC}
var
  LFormatSettings: TFormatSettings;
  {$ENDIF}
begin
  {$IFNDEF FPC}
  {$IFDEF DELPHIXE_UP}
  Result :=  FormatSettings.DecimalSeparator;
  {$ELSE}
  Result :=  DecimalSeparator;
  {$ENDIF}
  {$ELSE}
  LFormatSettings := DefaultFormatSettings;
  Result :=  LFormatSettings.DecimalSeparator;
  {$ENDIF}
end;


function FixedFloatToStr(const Value: Extended): string;
var
  lS: string;
begin
  lS := FloatToStr(Value);
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := LS;
  end
  else
  begin
    Result := StringReplace( lS,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             [rfReplaceAll]);
  end;
end;

{**
 * Fixed TryStrToFloat to convert dot (.) decimal separator to DecimalSeparator, TryStrToFloat expects
 * decimal separator to be DecimalSeparator, but JSON uses dot (.) as decimal separator.
 *}
function FixedTryStrToFloat(const S: string; out Value: Extended): Boolean;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := TryStrToFloat(S, Value);
  end
  else
  begin
    FixedS := StringReplace( S,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             [rfReplaceAll]);
    Result := TryStrToFloat(FixedS, Value);
  end;
end;

{**
 * Fixed StrToFloat to convert dot (.) decimal separator to DecimalSeparator, StrToFloat expects
 * decimal separator to be DecimalSeparator, but JSON uses dot (.) as decimal separator.
 *}
function FixedStrToFloat(const S: string): Extended;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := StrToFloat(S);
  end
  else
  begin
    FixedS := StringReplace( S,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             [rfReplaceAll]);
    Result := StrToFloat(FixedS);
  end;
end;

function InArray(Str : string; ary : array of String) : boolean;
var
  i: Integer;
begin
  Result := Length(ary)=0;
  for i := 0 to Length(ary) - 1 do
  begin
    if CompareText(ary[i],Str) = 0 then
    begin
      Result := True;
      break;
    end;
  end;
end;


{$IFDEF DELPHIXE6_UP}
function InternalObjectToJSON(Obj : Tobject; PropList : array of String; WriteClass : boolean = false) : String; overload;
const lcst_exceptheader = 'ObjectToJson : ';
var
  pl : PPropList;
  iCnt : integer;
  i: Integer;
  sVal : string;
  o : TObject;
  //dyn array.
  lTypeData: PTypeData;
  {$IFDEF FPC}
  P : Pointer;
  lTypeInfoFPC : PTypeInfo;
  {$ENDIF}
  lTypeInfo: PPTypeInfo;
  lpTypeInfo: PPPTypeInfo;
 // Prop: PPropInfo;
  j : integeR;
  arrobj : TObjectDynArray;
  arrstr : TStringDynArray;
  arrint : TIntegerDynArray;
  jc : Integer;
  jcs : String;

  js : TJson;

    Procedure RT;
    begin
      raise Exception.Create(lcst_exceptheader + 'Type must be implemented');
    end;


begin
  if not Assigned(obj) then
    raise Exception.Create(lcst_exceptheader + 'Input object is null');

  iCnt := GetPropList(Obj, pl);
  js :=  TJSon.Create;
  try
    Result := '{' {$IFDEF LINEBREAKJSONFORMAT}+ sLineBreak {$ENDIF};
    if WriteClass then
    begin
      Result := Result+'"class" : "'+js.Encode(obj.ClassName)+'"';
    end;
    for i := 0 to iCnt-1 do
    begin
      if not InArray(pl[i]^.Name, PropList) then
        Continue;
      sVal := '';
      case pl[i]^.PropType^.Kind of
        tkInteger: sVal := IntToStr(GetOrdProp(obj,pl[i]));
        tkFloat  :
        begin
          if pl[i]^.PropType^.Name = 'TDateTime' then
            sVal := JSONDateToString(GetFloatProp(obj,pl[i]))
          else if pl[i]^.PropType^.Name = 'TDate' then
            sVal := JSONDateToString(GetFloatProp(obj,pl[i]))
          else if pl[i]^.PropType^.Name = 'TTime' then
            sVal := JSONDateToString(GetFloatProp(obj,pl[i]))
          else
            sVal := FixedFloatToStr(GetFloatProp(obj,pl[i]));
        end;
        tkInt64  : sVal := IntToStr(GetInt64Prop(obj,pl[i]));

        tkChar   : sVal := '"'+js.Encode(Char(GetOrdProp(obj,pl[i])))+'"';
        {$IFDEF FPC}
        tkAString,
        {$ENDIF}
        tkLString,
        tkString,
        tkUString: sVal := '"'+js.Encode(GetStrProp(obj,pl[i]))+'"';
        tkWChar  : sVal := '"'+js.Encode(WideChar(GetOrdProp(obj,pl[i])))+'"';
        tkWString: sVal := '"'+js.Encode(GetWideStrProp(obj,pl[i]))+'"';
        tkEnumeration:
        begin
          sVal := GetEnumProp(obj,pl[i].Name);
          sVal := '"'+js.Encode(IntToStr(GetEnumValue(pl[i]^.PropType^,sVal)))+'"';     //GetEnumValue(pl[i]^.PropType^,GetEnumProp(obj,pl[i].Name))
        end;
        tkClass:
        begin
          o := GetObjectProp(Obj,pl[i]);
          if o is TObject then
            sVal := InternalObjectToJSON(TObject(o),PropList)
          else
            Continue;
        end;
        tkDynArray :
        begin
          sVal := '[ ';
          jcs :=',';

          lTypeData := GetTypeData(pl[i]^.PropType{$IFNDEF FPC}^{$ENDIF});
          {$IFNDEF FPC}
          lpTypeInfo :=  PPPTypeInfo(lTypeData^.DynUnitNameFld.Tail);
          lTypeInfo := lpTypeInfo^;
          case lTypeInfo^.Kind of
          {$ELSE}
          lTypeInfoFPC := lTypeData^.ElType2;
          case lTypeInfoFPC^.Kind of
            tkAString,
          {$ENDIF}
            tkUString, tkString : //Warning, take care of {$IFDEF} just upside :)
            begin
              arrstr := TStringDynArray(GetDynArrayProp(Obj, pl[i]));
              jc := Length(arrstr)-1;
              for j := 0 to Length(arrstr)-1 do
              begin
                if j=jc then
                  jcs := EmptyStr;
                sVal := sVal + js.Encode(arrstr[j]) + jcs;
              end;
            end;
            tkInteger :
            begin
              arrint := TIntegerDynArray(GetDynArrayProp(Obj, pl[i]));
              jc := Length(arrint)-1;
              for j := 0 to Length(arrint)-1 do
              begin
                if j=jc then
                  jcs := EmptyStr;
                sVal := sVal + js.Encode(IntToStr(arrint[j])) + jcs;
              end;
            end;
            tkClass :
            begin
              arrobj := TObjectDynArray(GetDynArrayProp(Obj, pl[i]));
              jc := Length(arrobj)-1;
              for j := 0 to Length(arrobj)-1 do
              begin
                if j=jc then
                  jcs := EmptyStr;
                sVal := sVal + InternalObjectToJSON(TObject(arrobj[j]),[]) + jcs;
              end;
            end;
          end;
          sVal :=sval +  ' ]';
        end;
        tkArray,
        tkUnknown,
        tkSet,
        tkMethod,
        tkVariant,
        tkRecord, //Record will not be supported because of discrepeancy between delphi and FPC for record rtti processing.
        tkInterface : RT;
      end;

      Result := Result + '"' + js.Encode(pl[i]^.Name)+'" : '+sVal;
      if Trim(Result) <> '{' then
      begin
        if i< icnt-1 then
        begin
            Result := Result+' , ' {$IFDEF LINEBREAKJSONFORMAT}+ sLineBreak {$ENDIF};
        end
        else
        begin
          if Trim(Result) <> '{' then
            Result := Result {$IFDEF LINEBREAKJSONFORMAT}+ sLineBreak {$ENDIF};
        end;
      end;
    end;
  finally
    FreeMem(pl);
    FreeAndNil(js);
  end;
  Result := Result+'}';
end;

Procedure InternalJsonToObject(Const aJsonString : String; Var aObject : TObject);
const lcst_exceptheader = 'JsonToObject : ';
var //Json stuffs
    lJSON : TJson;
    lJsValue : TJsonValue;
    lJsArray : TJsonArray;

    //rtti
    lpl : PPropList;
    lTypeData: PTypeData;
    {$IFDEF FPC}
    lTypeInfoFPC : PTypeInfo;
    {$ENDIF}
    lTypeInfo: PPTypeInfo;
    lpTypeInfo: PPPTypeInfo;

    //General
    lo : TObject;
    loClass : TClass;
    lDynObjArray : TObjectDynArray;
    lIntegerArray : TIntegerDynArray;
    lstringArray : TStringDynArray;
    lpc : Cardinal;
    i,j : integer;
    lsTemp : String;

    Procedure RT;
    begin
      raise Exception.Create(lcst_exceptheader + 'Type must be implemented');
    end;

begin
  Assert((assigned(aObject)));
  lJSON := TJson.Create;
  try
    lJSON.Parse(aJsonString);
    if Not(lJSON.StructType = TJsonStructType.jsObject) then
    begin
      raise Exception.Create(lcst_exceptheader + 'JSON Parser fails : Json file is not an object representation.');
    end;

    //JSON will drive by object structure.
    lpc := GetPropList(aObject, lpl);
    for i := 0 to lpc-1 do
    begin
      lJsValue := lJSON[lpl[i].Name];

      if lJsValue.IsNull then
      begin
        if lJsValue.IsEmpty then
        begin
          //JSON Porpety null, but exists.
          Continue;
        end
        else
        begin
          //Property is not in JSON,
          Continue;
        end;
      end;

      case lpl[i]^.PropType^.Kind of
        tkFloat  :
        begin
          if lJsValue.ValueType = jvString then //According to JSON, it is parhaps a date ? Rtti reconize date as float.
          begin
            lsTemp := lJSON[lpl[i].Name].AsString;
            if JSONStringIsCompatibleDate(lsTemp) then
            begin
              SetFloatProp(aObject,lpl[i].Name,JSONStringToDate(lsTemp));
            end
            Else
            begin
              raise Exception.Create(lcst_exceptheader + 'Incompatible type (Perhaps unknow date format) Property "'+lpl[i].Name+'"');
            end;
          end
          else
          begin
            SetFloatProp(aObject,lpl[i].Name,lJSON[lpl[i].Name].AsNumber);
          end;
        end;
        tkInt64  : SetInt64Prop(aObject,lpl[i].Name,lJSON[lpl[i].Name].AsInteger);
        tkInteger: SetOrdProp(aObject,lpl[i].Name,lJSON[lpl[i].Name].AsInteger);
        tkLString,
        tkString,
        tkUString,
        tkChar,
        tkWChar,
        {$IFDEF FPC}
        tkAString,
        {$ENDIF}
        tkWString:
        begin
          SetStrProp(aObject,lpl[i].Name,lJSON[lpl[i].Name].AsString);
        end;
        tkEnumeration: SetOrdProp(aObject,lpl[i].Name,Integer(lJSON[lpl[i].Name].AsInteger));
        tkClass:
        begin
          if (lJsValue.ValueType = TJsonValueType.jvObject) or (lJsValue.ValueType = TJsonValueType.jvNone) then
          begin
            //In jvNone case, we do nothing (JSON has not this property, but it is object which driven our build.
            if (lJsValue.ValueType = TJsonValueType.jvObject) then
            begin
              lTypeData := GetTypeData(lpl[i]^.PropType{$IFNDEF FPC}^{$ENDIF});
              loClass := lTypeData^.ClassType;
              lo := loClass.Create;
              try
                InternalJsonToObject(lJsValue.Stringify, lo);
              Except
                On E: Exception do
                  raise Exception.Create(lcst_exceptheader + '[InternalJsonToObject reentrance single object] (Property '+lpl[i].Name+') ' + E.Message);
              end;
              SetObjectProp(aObject,lpl[i]^.Name,lo);
            end;
          end
          else
          begin
            raise Exception.Create(lcst_exceptheader + 'Original JSON type not match with class type : Property "'+lpl[i].Name+'"');
          end;
        end;
        tkDynArray :
        begin
          if lJsValue.ValueType = TJsonValueType.jvArray then
          begin
            ljsArray := lJsValue.AsArray;
            for j := 0 to lJsArray.Count-1 do
            begin
              case lJsArray[j].ValueType of
                jvString :
                begin
                  SetLength(lstringArray,Length(lstringArray)+1);
                  lstringArray[Length(lstringArray)-1] := lJsArray[j].AsString;
                end;
                jvObject :
                begin
                  lTypeData := GetTypeData(lpl[i]^.PropType{$IFNDEF FPC}^{$ENDIF});
                  {$IFNDEF FPC}
                  //Delphi compiler : RTTI permit to get automaticaly dependance class.
                  lpTypeInfo :=  PPPTypeInfo(lTypeData^.DynUnitNameFld.Tail);
                  lTypeInfo := lpTypeInfo^;
                  if (lTypeInfo^.Kind = tkClass) then
                  begin
                    loClass := lTypeInfo^.TypeData^.ClassType;
                    //loClass := TGSJson.Configuration.GetPropertyConfiguration(lpl[i]^.Name).ItemArrayType; //as FPC ? switch ?
                  end
                  else
                  begin
                    raise Exception.Create(lcst_exceptheader + ' Delphi Class resolving : Not object Error : Property "'+lpl[i].Name+'"');
                  end;
                  {$ELSE}
                  //FPC side : first view not possible :( Use kind of marshaller config instead.
                  lTypeInfoFPC := lTypeData^.ElType2;
                  if (lTypeInfoFPC^.Kind = tkClass) then
                  begin
                    loClass := TGSJson.Configuration.GetPropertyConfiguration(lpl[i]^.Name).ItemArrayType;
                  end
                  else
                  begin
                    raise Exception.Create(lcst_exceptheader + ' FPC Class resolving : Not object Error : Property "'+lpl[i].Name+'"');
                  end;
                  {$ENDIF}
                  lo := loClass.Create;
                  try
                    InternalJsonToObject(lJsArray[j].Stringify, lo);
                    SetLength(lDynObjArray,Length(lDynObjArray)+1);
                    lDynObjArray[Length(lDynObjArray)-1] := lo;
                  Except
                    On E: EXception do
                      raise Exception.Create(lcst_exceptheader +'[InternalJsonToObject reentrance] : Property "'+lpl[i].Name+'" - ' + E.Message);
                  end;
                end;
                jvNumber:
                begin
                  SetLength(lIntegerArray,Length(lIntegerArray)+1);
                  lIntegerArray[Length(lIntegerArray)-1] := lJsArray[j].AsInteger;
                end
                else
                begin
                  raise Exception.Create(lcst_exceptheader + 'type not implemented or supported : Property "'+lpl[i].Name+'"');
                end;
              end;
            end;
            if lJsArray.Count>0 then
            begin
              case lJsArray[0].ValueType of
              jvString : SetDynArrayProp(aObject,lpl[i].Name,lstringArray);
              jvObject : SetDynArrayProp(aObject,lpl[i].Name,lDynObjArray);
              jvNumber : SetDynArrayProp(aObject,lpl[i].Name,lIntegerArray);
              end;
            end;
          end
          else
          begin
            //empty element.
            if Not(lJsValue.IsNull) then
            begin
              //element does not exists in JSON. error ?
              //Todo : Property like "StrictElementCorrespondaceCheck" something like that ?
              //raise Exception.Create('type Error Message');
            end;
          end;
        end;
        tkArray,
        tkUnknown,
        tkSet,
        tkMethod,
        tkVariant,
        tkRecord,
        tkInterface : RT;
      end;
    end;
  finally
    Dispose(lpl);
    FreeAndNil(lJSON);
  end;
end;

function __ObjectToJson(aObject: TObject): String;
begin
  Result := InternalObjectToJSON(aObject,[]);
end;

Procedure __jsonToObject(Const aJSONString : String; Var aObject : TObject);
begin
  InternalJsonToObject(aJSONString, aObject);
end;
{$ENDIF}

Initialization

JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;

Finalization

end.
