unit IniPersist;
//
// Copyright (c) 2009 - Robert Love
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
interface
uses SysUtils,Classes, Rtti,TypInfo;

type
  IniValueAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDefaultValue: string;
    FSection: string;
  published
     constructor Create(const aSection : String;const aName : string;const aDefaultValue : String = '');
     property Section : string read FSection write FSection;
     property Name : string read FName write FName;
     property DefaultValue : string read FDefaultValue write FDefaultValue;
  end;

  EIniPersist = class(Exception);

  TIniPersist = class (TObject)
  private
    class procedure SetValue(aData : String;var aValue : TValue);
    class function GetValue(var aValue : TValue) : String;
    class function GetIniAttribute(Obj : TRttiObject) : IniValueAttribute;
  public
    class procedure Load(FileName : String;obj : TObject);
    class procedure Save(FileName : String;obj : TObject);
  end;

implementation
 uses IniFiles;
{ TIniValue }

constructor IniValueAttribute.Create(const aSection, aName, aDefaultValue: String);
begin
  FSection := aSection;
  FName := aName;
  FDefaultValue := aDefaultValue;
end;

{ TIniPersist }

class function TIniPersist.GetIniAttribute(Obj: TRttiObject): IniValueAttribute;
var
 Attr: TCustomAttribute;
begin
 for Attr in Obj.GetAttributes do
 begin
    if Attr is IniValueAttribute then
    begin
      exit(IniValueAttribute(Attr));
    end;
 end;
 result := nil;
end;

class procedure TIniPersist.Load(FileName: String; obj: TObject);
var
 ctx      : TRttiContext;
 objType  : TRttiType;
 Field : TRttiField;
 Prop  : TRttiProperty;
 Value : TValue;
 IniValue : IniValueAttribute;
 Ini : TIniFile;
 Data : String;
begin
 ctx := TRttiContext.Create;
 try
   Ini := TIniFile.Create(FileName);
   try
     objType := ctx.GetType(Obj.ClassInfo);
     for Prop in objType.GetProperties do
     begin
       IniValue := GetIniAttribute(Prop);
       if Assigned(IniValue) then
       begin
          Data := Ini.ReadString(IniValue.Section,IniValue.Name,IniValue.DefaultValue);
          Value := Prop.GetValue(Obj);
          SetValue(Data,Value);
          Prop.SetValue(Obj,Value);
       end;
     end;
     for Field in objType.GetFields do
     begin
       IniValue := GetIniAttribute(Field);
       if Assigned(IniValue) then
       begin
          Data := Ini.ReadString(IniValue.Section,IniValue.Name,IniValue.DefaultValue);
          Value := Field.GetValue(Obj);
          SetValue(Data,Value);
          Field.SetValue(Obj,Value);
       end;
     end;
   finally
     Ini.Free;
   end;
 finally
   ctx.Free;
 end;
end;

class procedure TIniPersist.SetValue(aData: String;var aValue: TValue);
var
 I : Integer;
begin
 case aValue.Kind of
   tkWChar,
   tkLString,
   tkWString,
   tkString,
   tkChar,
   tkUString : aValue := aData;
   tkInteger,
   tkInt64  : aValue := StrToInt(aData);
   tkFloat  : aValue := StrToFloat(aData);
   tkEnumeration:  aValue := TValue.FromOrdinal(aValue.TypeInfo,GetEnumValue(aValue.TypeInfo,aData));
   tkSet: begin
             i :=  StringToSet(aValue.TypeInfo,aData);
             TValue.Make(@i, aValue.TypeInfo, aValue);
          end;
   else raise EIniPersist.Create('Type not Supported');
 end;
end;

class procedure TIniPersist.Save(FileName: String; obj: TObject);
var
 ctx      : TRttiContext;
 objType  : TRttiType;
 Field    : TRttiField;
 Prop     : TRttiProperty;
 Value    : TValue;
 IniValue : IniValueAttribute;
 Ini      : TIniFile;
 Data     : String;
begin
 ctx := TRttiContext.Create;
 try
   Ini := TIniFile.Create(FileName);
   try
     objType := ctx.GetType(Obj.ClassInfo);
     for Prop in objType.GetProperties do
     begin
       IniValue := GetIniAttribute(Prop);
       if Assigned(IniValue) then
       begin
          Value := Prop.GetValue(Obj);
          Data := GetValue(Value);
          if Data.Empty = '' then
            Ini.WriteString(IniValue.Section,IniValue.Name,IniValue.DefaultValue)
          else
            Ini.WriteString(IniValue.Section,IniValue.Name,data);
       end;
     end;
     for Field in objType.GetFields do
     begin
       IniValue := GetIniAttribute(Field);
       if Assigned(IniValue) then
       begin
          Value := Field.GetValue(Obj);
          Data := GetValue(Value);
          if Data.Empty = '' then
            Ini.WriteString(IniValue.Section,IniValue.Name,IniValue.DefaultValue)
          else
            Ini.WriteString(IniValue.Section,IniValue.Name,data);
       end;
     end;
   finally
     Ini.Free;
   end;
 finally
   ctx.Free;
 end;
end;

class function TIniPersist.GetValue(var aValue: TValue) : string;
begin
   if aValue.Kind in [tkWChar, tkLString, tkWString, tkString, tkChar,
                    tkUString, tkInteger, tkInt64, tkFloat, tkEnumeration,
                    tkSet]  then
   result := aValue.ToString
   else raise EIniPersist.Create('Type not Supported');
end;
end.
