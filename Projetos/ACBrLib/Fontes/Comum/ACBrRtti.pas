{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$goto on}
{$Assertions on}

unit ACBrRtti;
  
interface
 
uses
  Classes, SysUtils, TypInfo, rttiutils;

resourcestring
  SErrUnableToGetValueForType = 'Unable to get value for type %s';
  SErrUnableToSetValueForType = 'Unable to set value for type %s';
  SErrInvalidTypecast         = 'Invalid class typecast';

type

  IValueData = interface
    ['{1338B2F3-2C21-4798-A641-CA2BC5BF2396}']
    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: SizeInt;
    function GetReferenceToRawData: pointer;
  end;

  { TValueDataIntImpl }

  TValueDataIntImpl = class(TInterfacedObject, IValueData)
  private
    FBuffer: Pointer;
    FDataSize: SizeInt;
    FTypeInfo: PTypeInfo;
    FIsCopy: Boolean;
    FUseAddRef: Boolean;

  public
    constructor CreateCopy(ACopyFromBuffer: Pointer; ALen: SizeInt; ATypeInfo: PTypeInfo; AAddRef: Boolean);
    constructor CreateRef(AData: Pointer; ATypeInfo: PTypeInfo; AAddRef: Boolean);
    destructor Destroy; override;

    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: SizeInt;
    function GetReferenceToRawData: pointer;

  end;

  TValueData = record
    FTypeInfo: PTypeInfo;
    FValueData: IValueData;
    case integer of
      0:  (FAsUByte: Byte);
      1:  (FAsUWord: Word);
      2:  (FAsULong: LongWord);
      3:  (FAsObject: Pointer);
      4:  (FAsClass: TClass);
      5:  (FAsSByte: Shortint);
      6:  (FAsSWord: Smallint);
      7:  (FAsSLong: LongInt);
      8:  (FAsSingle: Single);
      9:  (FAsDouble: Double);
      10: (FAsExtended: Extended);
      11: (FAsComp: Comp);
      12: (FAsCurr: Currency);
      13: (FAsUInt64: QWord);
      14: (FAsSInt64: Int64);
      15: (FAsMethod: TMethod);
      16: (FAsPointer: Pointer);
      { FPC addition for open arrays }
      17: (FArrLength: SizeInt; FElSize: SizeInt);
  end;

  { TValue }

  TValue = record
  private
    FData: TValueData;
    function GetDataSize: SizeInt;
    function GetTypeDataProp: PTypeData; inline;
    function GetTypeInfo: PTypeInfo; inline;
    function GetTypeKind: TTypeKind; inline;
    function GetIsEmpty: boolean; inline;
  public
    class function Empty: TValue; static;
    class procedure Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue); static;
    { Note: a TValue based on an open array is only valid until the routine having the open array parameter is left! }
    class procedure MakeOpenArray(AArray: Pointer; ALength: SizeInt; ATypeInfo: PTypeInfo; out Result: TValue); static;
    generic class function From<T>(constref aValue: T): TValue; static; inline;
    { Note: a TValue based on an open array is only valid until the routine having the open array parameter is left! }
    generic class function FromOpenArray<T>(constref aValue: array of T): TValue; static; inline;
    class function FromOrdinal(aTypeInfo: PTypeInfo; aValue: Int64): TValue; static; {inline;}
    function IsArray: boolean; inline;
    function IsOpenArray: Boolean; inline;
    function AsString: string; inline;
    function AsUnicodeString: UnicodeString;
    function AsAnsiString: AnsiString;
    function AsExtended: Extended;
    function IsClass: boolean; inline;
    function AsClass: TClass;
    function IsObject: boolean; inline;
    function AsObject: TObject;
    function IsOrdinal: boolean; inline;
    function AsOrdinal: Int64;
    function AsBoolean: boolean;
    function AsCurrency: Currency;
    function AsInteger: Integer;
    function AsChar: Char; inline;
    function AsAnsiChar: AnsiChar;
    function AsWideChar: WideChar;
    function AsInt64: Int64;
    function AsUInt64: QWord;
    function AsInterface: IInterface;
    function ToString: String;
    function GetArrayLength: SizeInt;
    function GetArrayElement(AIndex: SizeInt): TValue;
    procedure SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
    function IsType(ATypeInfo: PTypeInfo): boolean; inline;
    generic function IsType<T>: Boolean; inline;
    function TryAsOrdinal(out AResult: int64): boolean;
    function GetReferenceToRawData: Pointer;
    procedure ExtractRawData(ABuffer: Pointer);
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    class operator := (const AValue: String): TValue; inline;
    class operator := (AValue: LongInt): TValue; inline;
    class operator := (AValue: Single): TValue; inline;
    class operator := (AValue: Double): TValue; inline;
{$ifdef FPC_HAS_TYPE_EXTENDED}
    class operator := (AValue: Extended): TValue; inline;
{$endif}
    class operator := (AValue: Currency): TValue; inline;
    class operator := (AValue: Comp): TValue; inline;
    class operator := (AValue: Int64): TValue; inline;
    class operator := (AValue: QWord): TValue; inline;
    class operator := (AValue: TObject): TValue; inline;
    class operator := (AValue: TClass): TValue; inline;
    class operator := (AValue: Boolean): TValue; inline;
    class operator := (AValue: IUnknown): TValue; inline;
    property DataSize: SizeInt read GetDataSize;
    property Kind: TTypeKind read GetTypeKind;
    property TypeData: PTypeData read GetTypeDataProp;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TValueArray = specialize TArray<TValue>;

   { TRttiMember }

  TMemberVisibility=(mvPrivate, mvProtected, mvPublic, mvPublished);

  { TRttiProperty }

  TRttiProperty = class
  private
    FPropInfo: PPropInfo;
    function GetIsWritable: boolean;
    function GetIsReadable: boolean;

  protected
    function GetVisibility: TMemberVisibility;
    function GetName: string;
    function GetHandle: Pointer;

  public
    constructor Create(APropInfo: PPropInfo);

    function GetValue(Instance: pointer): TValue;
    procedure SetValue(Instance: pointer; const AValue: TValue);

    property Handle: Pointer read GetHandle;
    property Name: string read GetName;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;

  end;
 
implementation

{ TValueDataIntImpl }

procedure IntFinalize(APointer, ATypeInfo: Pointer);
  external name 'FPC_FINALIZE';
procedure IntInitialize(APointer, ATypeInfo: Pointer);
  external name 'FPC_INITIALIZE';
procedure IntAddRef(APointer, ATypeInfo: Pointer);
  external name 'FPC_ADDREF';
function IntCopy(ASource, ADest, ATypeInfo: Pointer): SizeInt;
  external name 'FPC_COPY';

constructor TValueDataIntImpl.CreateCopy(ACopyFromBuffer: Pointer; ALen: SizeInt; ATypeInfo: PTypeInfo; AAddRef: Boolean);
begin
  FTypeInfo := ATypeInfo;
  FDataSize:=ALen;
  if ALen>0 then
    begin
      Getmem(FBuffer,FDataSize);
      if Assigned(ACopyFromBuffer) then
        system.move(ACopyFromBuffer^,FBuffer^,FDataSize)
      else
        FillChar(FBuffer^, FDataSize, 0);
    end;
  FIsCopy := True;
  FUseAddRef := AAddRef;
  if AAddRef and (ALen > 0) then begin
    if Assigned(ACopyFromBuffer) then
      IntAddRef(FBuffer, FTypeInfo)
    else
      IntInitialize(FBuffer, FTypeInfo);
  end;
end;

constructor TValueDataIntImpl.CreateRef(AData: Pointer; ATypeInfo: PTypeInfo; AAddRef: Boolean);
begin
  FTypeInfo := ATypeInfo;
  FDataSize := SizeOf(Pointer);
  if Assigned(AData) then
    FBuffer := PPointer(AData)^
  else
    FBuffer := Nil;
  FIsCopy := False;
  FUseAddRef := AAddRef;
  if AAddRef and Assigned(AData) then
    IntAddRef(@FBuffer, FTypeInfo);
end;

destructor TValueDataIntImpl.Destroy;
begin
  if Assigned(FBuffer) then begin
    if FUseAddRef then
      if FIsCopy then
        IntFinalize(FBuffer, FTypeInfo)
      else
        IntFinalize(@FBuffer, FTypeInfo);
    if FIsCopy then
      Freemem(FBuffer);
  end;
  inherited Destroy;
end;

procedure TValueDataIntImpl.ExtractRawData(ABuffer: pointer);
begin
  if FDataSize = 0 then
    Exit;
  if FIsCopy then
    System.Move(FBuffer^, ABuffer^, FDataSize)
  else
    System.Move(FBuffer{!}, ABuffer^, FDataSize);
  if FUseAddRef then
    IntAddRef(ABuffer, FTypeInfo);
end;

procedure TValueDataIntImpl.ExtractRawDataNoCopy(ABuffer: pointer);
begin
  if FDataSize = 0 then
    Exit;
  if FIsCopy then
    system.move(FBuffer^, ABuffer^, FDataSize)
  else
    System.Move(FBuffer{!}, ABuffer^, FDataSize);
end;

function TValueDataIntImpl.GetDataSize: SizeInt;
begin
  result := FDataSize;
end;

function TValueDataIntImpl.GetReferenceToRawData: pointer;
begin
  if FIsCopy then
    result := FBuffer
  else
    result := @FBuffer;
end;

{ TValue }

class function TValue.Empty: TValue;
begin
  result.FData.FTypeInfo := nil;
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
end;

function TValue.GetTypeDataProp: PTypeData;
begin
  result := GetTypeData(FData.FTypeInfo);
end;

function TValue.GetTypeInfo: PTypeInfo;
begin
  result := FData.FTypeInfo;
end;

function TValue.GetTypeKind: TTypeKind;
begin
  if not Assigned(FData.FTypeInfo) then
    Result := tkUnknown
  else
    result := FData.FTypeInfo^.Kind;
end;

function TValue.GetDataSize: SizeInt;
begin
  if Assigned(FData.FValueData) and (Kind <> tkSString) then
    Result := FData.FValueData.GetDataSize
  else begin
    Result := 0;
    case Kind of
      tkEnumeration,
      tkBool,
      tkInt64,
      tkQWord,
      tkInteger:
        case TypeData^.OrdType of
          otSByte,
          otUByte:
            Result := SizeOf(Byte);
          otSWord,
          otUWord:
            Result := SizeOf(Word);
          otSLong,
          otULong:
            Result := SizeOf(LongWord);
          otSQWord,
          otUQWord:
            Result := SizeOf(QWord);
        end;
      tkChar:
        Result := SizeOf(AnsiChar);
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Result := SizeOf(Single);
          ftDouble:
            Result := SizeOf(Double);
          ftExtended:
            Result := SizeOf(Extended);
          ftComp:
            Result := SizeOf(Comp);
          ftCurr:
            Result := SizeOf(Currency);
        end;
      tkSet:
        Result := TypeData^.SetSize;
      tkMethod:
        Result := SizeOf(TMethod);
      tkSString:
        { ShortString can hold max. 254 characters as [0] is Length and [255] is #0 }
        Result := SizeOf(ShortString) - 2;
      tkVariant:
        Result := SizeOf(Variant);
      tkProcVar:
        Result := SizeOf(CodePointer);
      tkWChar:
        Result := SizeOf(WideChar);
      tkUChar:
        Result := SizeOf(UnicodeChar);
      tkFile:
        { ToDo }
        Result := SizeOf(TTextRec);
      tkAString,
      tkWString,
      tkUString,
      tkInterface,
      tkDynArray,
      tkClass,
      tkHelper,
      tkClassRef,
      tkInterfaceRaw,
      tkPointer:
        Result := SizeOf(Pointer);
      tkObject,
      tkRecord:
        Result := TypeData^.RecSize;
      tkArray:
        Result := TypeData^.ArrayData.Size;
      tkUnknown,
      tkLString:
        Assert(False);
    end;
  end;
end;

class procedure TValue.Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue);
type
  PMethod = ^TMethod;
var
  td: PTypeData;
begin
  result.FData.FTypeInfo:=ATypeInfo;
  { resets the whole variant part; FValueData is already Nil }
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
  if not Assigned(ATypeInfo) then
    Exit;
  { first handle those types that need a TValueData implementation }
  case ATypeInfo^.Kind of
    tkSString  : begin
                   td := GetTypeData(ATypeInfo);
                   result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, td^.MaxLength + 1, ATypeInfo, True);
                 end;
    tkWString,
    tkUString,
    tkAString  : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkDynArray : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkArray    : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.ArrayData.Size, ATypeInfo, False);
    tkObject,
    tkRecord   : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.RecSize, ATypeInfo, False);
    tkInterface: result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
  end;
  if not Assigned(ABuffer) then
    Exit;
  { now handle those that are happy with the variant part of FData }
  case ATypeInfo^.Kind of
    tkSString,
    tkWString,
    tkUString,
    tkAString,
    tkDynArray,
    tkArray,
    tkObject,
    tkRecord,
    tkInterface:
      { ignore }
      ;
    tkClass    : result.FData.FAsObject := PPointer(ABuffer)^;
    tkClassRef : result.FData.FAsClass := PClass(ABuffer)^;
    tkInterfaceRaw : result.FData.FAsPointer := PPointer(ABuffer)^;
    tkInt64    : result.FData.FAsSInt64 := PInt64(ABuffer)^;
    tkQWord    : result.FData.FAsUInt64 := PQWord(ABuffer)^;
    tkProcVar  : result.FData.FAsMethod.Code := PCodePointer(ABuffer)^;
    tkMethod   : result.FData.FAsMethod := PMethod(ABuffer)^;
    tkPointer  : result.FData.FAsPointer := PPointer(ABuffer)^;
    tkSet      : begin
                   td := GetTypeData(ATypeInfo);
                   case td^.OrdType of
                     otUByte: begin
                       { this can either really be 1 Byte or a set > 32-bit, so
                         check the underlying type }
                       if not (td^.CompType^.Kind in [tkInteger,tkEnumeration]) then
                         raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
                       case td^.SetSize of
                         0, 1:
                           Result.FData.FAsUByte := PByte(ABuffer)^;
                         { these two cases shouldn't happen, but better safe than sorry... }
                         2:
                           Result.FData.FAsUWord := PWord(ABuffer)^;
                         3, 4:
                           Result.FData.FAsULong := PLongWord(ABuffer)^;
                         { maybe we should also allow storage as otUQWord? }
                         5..8:
                           Result.FData.FAsUInt64 := PQWord(ABuffer)^;
                         else
                           Result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, td^.SetSize, ATypeInfo, False);
                       end;
                     end;
                     otUWord:
                       Result.FData.FAsUWord := PWord(ABuffer)^;
                     otULong:
                       Result.FData.FAsULong := PLongWord(ABuffer)^;
                     else
                       { ehm... Panic? }
                       raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
                   end;
                 end;
    tkChar,
    tkWChar,
    tkUChar,
    tkEnumeration,
    tkInteger  : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otSByte: result.FData.FAsSByte := PShortInt(ABuffer)^;
                     otUByte: result.FData.FAsUByte := PByte(ABuffer)^;
                     otSWord: result.FData.FAsSWord := PSmallInt(ABuffer)^;
                     otUWord: result.FData.FAsUWord := PWord(ABuffer)^;
                     otSLong: result.FData.FAsSLong := PLongInt(ABuffer)^;
                     otULong: result.FData.FAsULong := PLongWord(ABuffer)^;
                   end;
                 end;
    tkBool     : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otUByte: result.FData.FAsUByte := Byte(System.PBoolean(ABuffer)^);
                     otUWord: result.FData.FAsUWord := Word(PBoolean16(ABuffer)^);
                     otULong: result.FData.FAsULong := DWord(PBoolean32(ABuffer)^);
                     otUQWord: result.FData.FAsUInt64 := QWord(PBoolean64(ABuffer)^);
                     otSByte: result.FData.FAsSByte := ShortInt(PByteBool(ABuffer)^);
                     otSWord: result.FData.FAsSWord := SmallInt(PWordBool(ABuffer)^);
                     otSLong: result.FData.FAsSLong := LongInt(PLongBool(ABuffer)^);
                     otSQWord: result.FData.FAsSInt64 := Int64(PQWordBool(ABuffer)^);
                   end;
                 end;
    tkFloat    : begin
                   case GetTypeData(ATypeInfo)^.FloatType of
                     ftCurr   : result.FData.FAsCurr := PCurrency(ABuffer)^;
                     ftSingle : result.FData.FAsSingle := PSingle(ABuffer)^;
                     ftDouble : result.FData.FAsDouble := PDouble(ABuffer)^;
                     ftExtended: result.FData.FAsExtended := PExtended(ABuffer)^;
                     ftComp   : result.FData.FAsComp := PComp(ABuffer)^;
                   end;
                 end;
  else
    raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
  end;
end;

class procedure TValue.MakeOpenArray(AArray: Pointer; ALength: SizeInt; ATypeInfo: PTypeInfo; out Result: TValue);
var
  el: TValue;
begin
  Result.FData.FTypeInfo := ATypeInfo;
  { resets the whole variant part; FValueData is already Nil }
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
  if not Assigned(ATypeInfo) then
    Exit;
  if ATypeInfo^.Kind <> tkArray then
    Exit;
  if not Assigned(AArray) then
    Exit;
  if ALength < 0 then
    Exit;
  Result.FData.FValueData := TValueDataIntImpl.CreateRef(@AArray, ATypeInfo, False);
  Result.FData.FArrLength := ALength;
  Make(Nil, Result.TypeData^.ArrayData.ElType, el);
  Result.FData.FElSize := el.DataSize;
end;

generic class function TValue.From<T>(constref aValue: T): TValue;
begin
  TValue.Make(@aValue, PTypeInfo(System.TypeInfo(T)), Result);
end;

generic class function TValue.FromOpenArray<T>(constref aValue: array of T): TValue;
var
  arrdata: Pointer;
begin
  if Length(aValue) > 0 then
    arrdata := @aValue[0]
  else
    arrdata := Nil;
  TValue.MakeOpenArray(arrdata, Length(aValue), PTypeInfo(System.TypeInfo(aValue)), Result);
end;

class function TValue.FromOrdinal(aTypeInfo: PTypeInfo; aValue: Int64): TValue;
{$ifdef ENDIAN_BIG}
var
  p: PByte;
  td: PTypeData;
{$endif}
begin
  if not Assigned(aTypeInfo) or
      not (aTypeInfo^.Kind in [tkInteger, tkInt64, tkQWord, tkEnumeration, tkBool, tkChar, tkWChar, tkUChar]) then
    raise EInvalidCast.Create(SErrInvalidTypecast);

{$ifdef ENDIAN_BIG}
  td := GetTypeData(aTypeInfo);
  p := @aValue;
  case td^.OrdType of
    otSByte,
    otUByte:
      p := p + 7;
    otSWord,
    otUWord:
      p := p + 6;
    otSLong,
    otULong:
      p := p + 4;
    otSQWord,
    otUQWord: ;
  end;
  TValue.Make(p, aTypeInfo, Result);
{$else}
  TValue.Make(@aValue, aTypeInfo, Result);
{$endif}
end;

function TValue.GetIsEmpty: boolean;
begin
  result := (FData.FTypeInfo=nil) or
            ((Kind in [tkSString, tkObject, tkRecord, tkArray]) and not Assigned(FData.FValueData)) or
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw]) and not Assigned(FData.FAsPointer));
end;

function TValue.IsArray: boolean;
begin
  result := kind in [tkArray, tkDynArray];
end;

function TValue.IsOpenArray: Boolean;
var
  td: PTypeData;
begin
  td := TypeData;
  Result := (Kind = tkArray) and (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0)
end;

function TValue.AsString: string;
begin
  if System.GetTypeKind(String) = tkUString then
    Result := String(AsUnicodeString)
  else
    Result := String(AsAnsiString);
end;

function TValue.AsUnicodeString: UnicodeString;
begin
  if (Kind in [tkSString, tkAString, tkUString, tkWString]) and not Assigned(FData.FValueData) then
    Result := ''
  else
    case Kind of
      tkSString:
        Result := UnicodeString(PShortString(FData.FValueData.GetReferenceToRawData)^);
      tkAString:
        Result := UnicodeString(PAnsiString(FData.FValueData.GetReferenceToRawData)^);
      tkWString:
        Result := UnicodeString(PWideString(FData.FValueData.GetReferenceToRawData)^);
      tkUString:
        Result := UnicodeString(PUnicodeString(FData.FValueData.GetReferenceToRawData)^);
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
end;

function TValue.AsAnsiString: AnsiString;
begin
  if (Kind in [tkSString, tkAString, tkUString, tkWString]) and not Assigned(FData.FValueData) then
    Result := ''
  else
    case Kind of
      tkSString:
        Result := AnsiString(PShortString(FData.FValueData.GetReferenceToRawData)^);
      tkAString:
        Result := AnsiString(PAnsiString(FData.FValueData.GetReferenceToRawData)^);
      tkWString:
        Result := AnsiString(PWideString(FData.FValueData.GetReferenceToRawData)^);
      tkUString:
        Result := AnsiString(PUnicodeString(FData.FValueData.GetReferenceToRawData)^);
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
end;

function TValue.AsExtended: Extended;
begin
  if Kind = tkFloat then
    begin
    case TypeData^.FloatType of
      ftSingle   : result := FData.FAsSingle;
      ftDouble   : result := FData.FAsDouble;
      ftExtended : result := FData.FAsExtended;
      ftCurr     : result := FData.FAsCurr;
      ftComp     : result := FData.FAsComp;
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
    end
  else if Kind in [tkInteger, tkInt64, tkQWord] then
    Result := AsInt64
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.IsObject: boolean;
begin
  result := (Kind = tkClass) or ((Kind = tkUnknown) and not Assigned(FData.FAsObject));
end;

function TValue.IsClass: boolean;
begin
  result := (Kind = tkClassRef) or ((Kind in [tkClass,tkUnknown]) and not Assigned(FData.FAsObject));
end;

function TValue.IsOrdinal: boolean;
begin
  result := (Kind in [tkInteger, tkInt64, tkQWord, tkBool, tkEnumeration, tkChar, tkWChar, tkUChar]) or
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw, tkUnknown]) and not Assigned(FData.FAsPointer));
end;

function TValue.IsType(ATypeInfo: PTypeInfo): boolean;
begin
  result := ATypeInfo = TypeInfo;
end;

generic function TValue.IsType<T>: Boolean;
begin
  Result := IsType(PTypeInfo(System.TypeInfo(T)));
end;

function TValue.AsObject: TObject;
begin
  if IsObject or (IsClass and not Assigned(FData.FAsObject)) then
    result := TObject(FData.FAsObject)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsClass: TClass;
begin
  if IsClass then
    result := FData.FAsClass
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsBoolean: boolean;
begin
  if (Kind = tkBool) then
    case TypeData^.OrdType of
      otSByte:  Result := ByteBool(FData.FAsSByte);
      otUByte:  Result := Boolean(FData.FAsUByte);
      otSWord:  Result := WordBool(FData.FAsSWord);
      otUWord:  Result := Boolean16(FData.FAsUWord);
      otSLong:  Result := LongBool(FData.FAsSLong);
      otULong:  Result := Boolean32(FData.FAsULong);
      otSQWord: Result := QWordBool(FData.FAsSInt64);
      otUQWord: Result := Boolean64(FData.FAsUInt64);
    end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsOrdinal: Int64;
begin
  if IsOrdinal then
    if Kind in [tkClass, tkClassRef, tkInterfaceRaw, tkUnknown] then
      Result := 0
    else
      case TypeData^.OrdType of
        otSByte:  Result := FData.FAsSByte;
        otUByte:  Result := FData.FAsUByte;
        otSWord:  Result := FData.FAsSWord;
        otUWord:  Result := FData.FAsUWord;
        otSLong:  Result := FData.FAsSLong;
        otULong:  Result := FData.FAsULong;
        otSQWord: Result := FData.FAsSInt64;
        otUQWord: Result := FData.FAsUInt64;
      end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsCurrency: Currency;
begin
  if (Kind = tkFloat) and (TypeData^.FloatType=ftCurr) then
    result := FData.FAsCurr
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInteger: Integer;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsAnsiChar: AnsiChar;
begin
  if Kind = tkChar then
    Result := Chr(FData.FAsUByte)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsWideChar: WideChar;
begin
  if Kind = tkWChar then
    Result := WideChar(FData.FAsUWord)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsChar: Char;
begin
{$if SizeOf(Char) = 1}
  Result := AsAnsiChar;
{$else}
  Result := AsWideChar;
{$endif}
end;

function TValue.AsInt64: Int64;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else if (Kind = tkFloat) and (TypeData^.FloatType = ftComp) then
    Result := Int64(FData.FAsComp)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsUInt64: QWord;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else if (Kind = tkFloat) and (TypeData^.FloatType = ftComp) then
    Result := QWord(FData.FAsComp)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInterface: IInterface;
begin
  if Kind = tkInterface then
    Result := PInterface(FData.FValueData.GetReferenceToRawData)^
  else if (Kind in [tkClass, tkClassRef, tkUnknown]) and not Assigned(FData.FAsPointer) then
    Result := Nil
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.ToString: String;
begin
  case Kind of
    tkWString,
    tkUString : result := AsUnicodeString;
    tkSString,
    tkAString : result := AsAnsiString;
    tkInteger : result := IntToStr(AsInteger);
    tkQWord   : result := IntToStr(AsUInt64);
    tkInt64   : result := IntToStr(AsInt64);
    tkBool    : result := BoolToStr(AsBoolean, True);
    tkPointer : result := '(pointer @ ' + HexStr(FData.FAsPointer) + ')';
    tkInterface : result := '(interface @ ' + HexStr(PPointer(FData.FValueData.GetReferenceToRawData)^) + ')';
    tkInterfaceRaw : result := '(raw interface @ ' + HexStr(FData.FAsPointer) + ')';
    tkEnumeration: Result := GetEnumName(TypeInfo, Integer(AsOrdinal));
    tkChar: Result := AnsiChar(FData.FAsUByte);
    tkWChar: Result := UTF8Encode(WideChar(FData.FAsUWord));
  else
    result := '';
  end;
end;

function TValue.GetArrayLength: SizeInt;
var
  td: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then
    Result := DynArraySize(PPointer(FData.FValueData.GetReferenceToRawData)^)
  else begin
    td := TypeData;
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then
      Result := FData.FArrLength
    else
      Result := td^.ArrayData.ElCount;
  end;
end;

function TValue.GetArrayElement(AIndex: SizeInt): TValue;
var
  data: Pointer;
  eltype: PTypeInfo;
  elsize: SizeInt;
  td: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then begin
    data := DynArrayIndex(PPointer(FData.FValueData.GetReferenceToRawData)^, [AIndex], FData.FTypeInfo);
    eltype := TypeData^.elType2;
  end else begin
    td := TypeData;
    eltype := td^.ArrayData.ElType;
    { open array? }
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then begin
      data := PPointer(FData.FValueData.GetReferenceToRawData)^;
      elsize := FData.FElSize
    end else begin
      data := FData.FValueData.GetReferenceToRawData;
      elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
    end;
    data := PByte(data) + AIndex * elsize;
  end;
  { MakeWithoutCopy? }
  Make(data, eltype, Result);
end;

procedure TValue.SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
var
  data: Pointer;
  eltype: PTypeInfo;
  elsize: SizeInt;
  td, tdv: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then begin
    data := DynArrayIndex(PPointer(FData.FValueData.GetReferenceToRawData)^, [AIndex], FData.FTypeInfo);
    eltype := TypeData^.elType2;
  end else begin
    td := TypeData;
    eltype := td^.ArrayData.ElType;
    { open array? }
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then begin
      data := PPointer(FData.FValueData.GetReferenceToRawData)^;
      elsize := FData.FElSize
    end else begin
      data := FData.FValueData.GetReferenceToRawData;
      elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
    end;
    data := PByte(data) + AIndex * elsize;
  end;
  { maybe we'll later on allow some typecasts, but for now be restrictive }
  if eltype^.Kind <> AValue.Kind then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  td := GetTypeData(eltype);
  tdv := AValue.TypeData;
  if ((eltype^.Kind in [tkInteger, tkBool, tkEnumeration, tkSet]) and (td^.OrdType <> tdv^.OrdType)) or
      ((eltype^.Kind = tkFloat) and (td^.FloatType <> tdv^.FloatType)) then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Assigned(AValue.FData.FValueData) and (eltype^.Kind <> tkSString) then
    IntCopy(AValue.FData.FValueData.GetReferenceToRawData, data, eltype)
  else
    Move(AValue.GetReferenceToRawData^, data^, AValue.DataSize);
end;

function TValue.TryAsOrdinal(out AResult: int64): boolean;
begin
  result := IsOrdinal;
  if result then
    AResult := AsOrdinal;
end;

function TValue.GetReferenceToRawData: Pointer;
begin
  if not Assigned(FData.FTypeInfo) then
    Result := Nil
  else if Assigned(FData.FValueData) then
    Result := FData.FValueData.GetReferenceToRawData
  else begin
    Result := Nil;
    case Kind of
      tkInteger,
      tkEnumeration,
      tkInt64,
      tkQWord,
      tkBool:
        case TypeData^.OrdType of
          otSByte:
            Result := @FData.FAsSByte;
          otUByte:
            Result := @FData.FAsUByte;
          otSWord:
            Result := @FData.FAsSWord;
          otUWord:
            Result := @FData.FAsUWord;
          otSLong:
            Result := @FData.FAsSLong;
          otULong:
            Result := @FData.FAsULong;
          otSQWord:
            Result := @FData.FAsSInt64;
          otUQWord:
            Result := @FData.FAsUInt64;
        end;
      tkSet: begin
        case TypeData^.OrdType of
          otUByte: begin
            case TypeData^.SetSize of
              1:
                Result := @FData.FAsUByte;
              2:
                Result := @FData.FAsUWord;
              3, 4:
                Result := @FData.FAsULong;
              5..8:
                Result := @FData.FAsUInt64;
              else
                { this should have gone through FAsValueData :/ }
                Result := Nil;
            end;
          end;
          otUWord:
            Result := @FData.FAsUWord;
          otULong:
            Result := @FData.FAsULong;
          else
            Result := Nil;
        end;
      end;
      tkChar:
        Result := @FData.FAsUByte;
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Result := @FData.FAsSingle;
          ftDouble:
            Result := @FData.FAsDouble;
          ftExtended:
            Result := @FData.FAsExtended;
          ftComp:
            Result := @FData.FAsComp;
          ftCurr:
            Result := @FData.FAsCurr;
        end;
      tkMethod:
        Result := @FData.FAsMethod;
      tkClass:
        Result := @FData.FAsObject;
      tkWChar:
        Result := @FData.FAsUWord;
      tkInterfaceRaw:
        Result := @FData.FAsPointer;
      tkProcVar:
        Result := @FData.FAsMethod.Code;
      tkUChar:
        Result := @FData.FAsUWord;
      tkFile:
        Result := @FData.FAsPointer;
      tkClassRef:
        Result := @FData.FAsClass;
      tkPointer:
        Result := @FData.FAsPointer;
      tkVariant,
      tkDynArray,
      tkArray,
      tkObject,
      tkRecord,
      tkInterface,
      tkSString,
      tkLString,
      tkAString,
      tkUString,
      tkWString:
        Assert(false, 'Managed/complex type not handled through IValueData');
    end;
  end;
end;

procedure TValue.ExtractRawData(ABuffer: Pointer);
begin
  if Assigned(FData.FValueData) then
    FData.FValueData.ExtractRawData(ABuffer)
  else if Assigned(FData.FTypeInfo) then
    Move((@FData.FAsPointer)^, ABuffer^, DataSize);
end;

procedure TValue.ExtractRawDataNoCopy(ABuffer: Pointer);
begin
  if Assigned(FData.FValueData) then
    FData.FValueData.ExtractRawDataNoCopy(ABuffer)
  else if Assigned(FData.FTypeInfo) then
    Move((@FData.FAsPointer)^, ABuffer^, DataSize);
end;

class operator TValue.:=(const AValue: String): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: LongInt): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Single): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Double): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
class operator TValue.:=(AValue: Extended): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;
{$endif}

class operator TValue.:=(AValue: Currency): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Comp): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Int64): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: QWord): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: TObject): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: TClass): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Boolean): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: IUnknown): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

{ TRttiProperty }

constructor TRttiProperty.Create(APropInfo: PPropInfo);
begin
  FPropInfo := APropInfo;
end;

function TRttiProperty.GetIsReadable: boolean;
begin
  result := assigned(FPropInfo^.GetProc);
end;

function TRttiProperty.GetIsWritable: boolean;
begin
  result := assigned(FPropInfo^.SetProc);
end;

function TRttiProperty.GetVisibility: TMemberVisibility;
begin
  // At this moment only pulished rtti-property-info is supported by fpc
  result := mvPublished;
end;

function TRttiProperty.GetName: string;
begin
  Result:=FPropInfo^.Name;
end;

function TRttiProperty.GetHandle: Pointer;
begin
  Result := FPropInfo;
end;

function TRttiProperty.GetValue(Instance: pointer): TValue;

  procedure ValueFromBool(value: Int64);
  var
    b8: Boolean;
    b16: Boolean16;
    b32: Boolean32;
    bb: ByteBool;
    bw: WordBool;
    bl: LongBool;
    td: PTypeData;
    p: Pointer;
  begin
    td := GetTypeData(FPropInfo^.PropType);
    case td^.OrdType of
      otUByte:
        begin
          b8 := Boolean(value);
          p := @b8;
        end;
      otUWord:
        begin
          b16 := Boolean16(value);
          p := @b16;
        end;
      otULong:
        begin
          b32 := Boolean32(value);
          p := @b32;
        end;
      otSByte:
        begin
          bb := ByteBool(value);
          p := @bb;
        end;
      otSWord:
        begin
          bw := WordBool(value);
          p := @bw;
        end;
      otSLong:
        begin
          bl := LongBool(value);
          p := @bl;
        end;
    end;
    TValue.Make(p, FPropInfo^.PropType, result);
  end;

  procedure ValueFromInt(value: Int64);
  var
    i8: UInt8;
    i16: UInt16;
    i32: UInt32;
    td: PTypeData;
    p: Pointer;
  begin
    td := GetTypeData(FPropInfo^.PropType);
    case td^.OrdType of
      otUByte,
      otSByte:
        begin
          i8 := value;
          p := @i8;
        end;
      otUWord,
      otSWord:
        begin
          i16 := value;
          p := @i16;
        end;
      otULong,
      otSLong:
        begin
          i32 := value;
          p := @i32;
        end;
    end;
    TValue.Make(p, FPropInfo^.PropType, result);
  end;

var
  Values: record
    case Integer of
      0: (Enum: Int64);
      1: (Bool: Int64);
      2: (Int: Int64);
      3: (Ch: Byte);
      4: (Wch: Word);
      5: (I64: Int64);
      6: (Si: Single);
      7: (Db: Double);
      8: (Ex: Extended);
      9: (Cur: Currency);
     10: (Cp: Comp);
     11: (A: Pointer;)
  end;
  s: String;
  ss: ShortString;
  O: TObject;
  Int: IUnknown;
begin
  case FPropinfo^.PropType^.Kind of
    tkSString:
      begin
        ss := ShortString(GetStrProp(TObject(Instance), FPropInfo));
        TValue.Make(@ss, FPropInfo^.PropType, result);
      end;
    tkAString:
      begin
        s := GetStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@s, FPropInfo^.PropType, result);
      end;
    tkEnumeration:
      begin
        Values.Enum := Integer(GetOrdProp(TObject(Instance), FPropInfo));
        ValueFromInt(Values.Enum);
      end;
    tkBool:
      begin
        Values.Bool := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromBool(Values.Bool);
      end;
    tkInteger:
      begin
        Values.Int := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromInt(Values.Int);
      end;
    tkChar:
      begin
        Values.Ch := Byte(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@Values.Ch, FPropInfo^.PropType, result);
      end;
    tkWChar:
      begin
        Values.Wch := Word(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@Values.Wch, FPropInfo^.PropType, result);
      end;
    tkInt64,
    tkQWord:
      begin
        Values.I64 := GetOrdProp(TObject(Instance), FPropInfo);
        TValue.Make(@Values.I64, FPropInfo^.PropType, result);
      end;
    tkClass:
    begin
      O := GetObjectProp(TObject(Instance), FPropInfo);
      TValue.Make(@O, FPropInfo^.PropType, Result);
    end;
    tkInterface:
    begin
      Int := GetInterfaceProp(TObject(Instance), FPropInfo);
      TValue.Make(@Int, FPropInfo^.PropType, Result);
    end;
    tkFloat:
    begin
      case GetTypeData(FPropInfo^.PropType)^.FloatType of
        ftCurr   :
          begin
            Values.Cur := Currency(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Cur, FPropInfo^.PropType, Result);
          end;
        ftSingle :
          begin
            Values.Si := Single(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Si, FPropInfo^.PropType, Result);
          end;
        ftDouble :
          begin
            Values.Db := Double(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Db, FPropInfo^.PropType, Result);
          end;
        ftExtended:
          begin
            Values.Ex := GetFloatProp(TObject(Instance), FPropInfo);
            TValue.Make(@Values.Ex, FPropInfo^.PropType, Result);
          end;
        ftComp   :
          begin
            Values.Cp := Comp(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Cp, FPropInfo^.PropType, Result);
          end;
      end;
    end;
    tkDynArray:
      begin
        Values.A := GetDynArrayProp(TObject(Instance), FPropInfo);
        TValue.Make(@Values.A, FPropInfo^.PropType, Result);
      end
  else
    result := TValue.Empty;
  end
end;

procedure TRttiProperty.SetValue(Instance: pointer; const AValue: TValue);
begin
  case FPropinfo^.PropType^.Kind of
    tkSString,
    tkAString:
      SetStrProp(TObject(Instance), FPropInfo, AValue.AsString);
    tkInteger,
    tkInt64,
    tkQWord,
    tkChar,
    tkBool,
    tkWChar,
    tkEnumeration:
      SetOrdProp(TObject(Instance), FPropInfo, AValue.AsOrdinal);
    tkClass:
      SetObjectProp(TObject(Instance), FPropInfo, AValue.AsObject);
    tkInterface:
      SetInterfaceProp(TObject(Instance), FPropInfo, AValue.AsInterface);
    tkFloat:
      SetFloatProp(TObject(Instance), FPropInfo, AValue.AsExtended);
    tkDynArray:
      SetDynArrayProp(TObject(Instance), FPropInfo, PPointer(AValue.GetReferenceToRawData)^);
  else
    raise exception.createFmt(SErrUnableToSetValueForType, [Name]);
  end
end;

end.
