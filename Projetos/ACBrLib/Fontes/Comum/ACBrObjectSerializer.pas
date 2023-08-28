{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrObjectSerializer;

interface

uses
  SysUtils, Classes, laz2_DOM, laz2_XMLWrite,
  inifiles, fpjson, jsonparser, TypInfo,
  rttiutils, ACBrUtil.Strings, ACBrLibResposta;

const
  CSessionFormat = '%s%.3d';
  CSufixFormat = '%.3d';

type
  { TACBrObjectSerializer }
  TACBrObjectSerializer = class
  private
    class function  GerarXml(Item: TACBrLibRespostaBase): Ansistring;
    class procedure GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode; const Target: TObject);
    class function  GerarIni(Item: TACBrLibRespostaBase): Ansistring;
    class procedure GravarIni(const AIni: TCustomIniFile; const ASessao: String; const Target: TObject;
                              const ASufix: string = '');
    class function  GerarJson(Item: TACBrLibRespostaBase): Ansistring;
    class procedure GravarJson(const JSON: TJSONObject; const ASessao: String; const Target: TObject);

  public
    class function Gerar<T: TACBrLibRespostaBase>(const Item: T; Tipo: TACBrLibRespostaTipo; Formato:
                         TACBrLibCodificacao = codUTF8): Ansistring; overload;
    class function Gerar<T: TACBrLibRespostaBase>(const Items: TArray<T>; Tipo: TACBrLibRespostaTipo;
                         Formato: TACBrLibCodificacao = codUTF8): Ansistring; overload;

  end;

implementation

uses
  ACBrRtti, ACBrLibHelpers, DateUtils;

{ TACBrObjectSerializer }

class function TACBrObjectSerializer.Gerar<T>(const Item: T; Tipo: TACBrLibRespostaTipo;
  Formato: TACBrLibCodificacao): Ansistring;
begin
  case Tipo of
    resXML: Result := TACBrObjectSerializer.GerarXml(Item);
    resJSON: Result := TACBrObjectSerializer.GerarJson(Item);
    else
      Result := TACBrObjectSerializer.GerarIni(Item);
  end;

  if Formato = codANSI then
    Result := ACBrUTF8ToAnsi(Result);
end;

class function TACBrObjectSerializer.Gerar<T>(const Items: TArray<T>; Tipo: TACBrLibRespostaTipo;
  Formato: TACBrLibCodificacao = codUTF8): Ansistring;
Var
  I: Integer;
  Item: TACBrLibRespostaBase;
  LJson : String;
begin
  Result := '';
  For I := 0 to High(Items) do
  begin
    Item := Items[I] as TACBrLibRespostaBase;
    case Tipo of
      resXML: Result := Result + TACBrObjectSerializer.GerarXml(Item);
      resJSON: 
        begin
          LJson := ifThen<String>(Result = '', '', Result + ',' );
          Result := LJson + TACBrObjectSerializer.GerarJson(Item);
        end;
    else
      Result := Result + TACBrObjectSerializer.GerarIni(Item);
    end;
  end;

  case Tipo of
    resXML: Result := '<Items>' + Result + '</Items>';
    resJSON: Result := '[' + Result + ']';
  end;

  if Formato = codANSI then
    Result := ACBrUTF8ToAnsi(Result);
end;

class function TACBrObjectSerializer.GerarXml(Item: TACBrLibRespostaBase): Ansistring;
var
  xDoc: TXMLDocument;
  RootNode: TDomNode;
  Stream: TMemoryStream;
begin
  xDoc := TXMLDocument.Create;

  try
    RootNode := xDoc.CreateElement(Item.Sessao);
    xDoc.AppendChild(RootNode);
    RootNode := xDoc.DocumentElement;
    GravarXml(xDoc, RootNode, Item);
    Stream := TMemoryStream.Create();
    WriteXML(xDoc.FirstChild, Stream);
    SetString(Result, PChar(Stream.Memory), Stream.Size div SizeOf(char));
  finally
    if Stream <> nil then
      Stream.Free;
    if xDoc <> nil then
      xDoc.Free;
  end;
end;

class procedure TACBrObjectSerializer.GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode;
  const Target: TObject);
Var
  PropList: TPropInfoList;
  i: Integer;
  ParentNode, ListNode: TDomNode;
  ClassObject: TObject;
  CollectionObject: TCollection;
  CollectionItem: TCollectionItem;
  ListObject: TList;
  ListItem: TObject;
  FValue: Extended;
  Propertie: TRttiProperty;
  AValue: TValue;
begin
  PropList := TPropInfoList.Create(Target, tkProperties);
  try
    for Propertie in PropList.GetProperties do
    begin
      AValue := Propertie.GetValue(Target);
      case AValue.Kind of
       tkClass:
          begin
            if not AValue.IsObject then continue;

            ClassObject := AValue.AsObject;
            if not Assigned(ClassObject) or (ClassObject = nil) then continue;

            if (ClassObject.InheritsFrom(TCollection)) then
            begin
              ParentNode := xDoc.CreateElement('Itens');

              CollectionObject := TCollection(ClassObject);
              for i := 0 to CollectionObject.Count - 1 do
              begin
                CollectionItem := CollectionObject.Items[i];
                ListNode := xDoc.CreateElement(Propertie.Name);
                GravarXml(xDoc, ListNode, CollectionItem);
                ParentNode.AppendChild(ListNode);
              end;
            end
            else if (ClassObject.InheritsFrom(TList)) then
            begin
              ParentNode := xDoc.CreateElement('Itens');

              ListObject := TList(ClassObject);
              for i := 0 to ListObject.Count - 1 do
              begin
                ListItem := ListObject.Items[i];

                if (ListItem.InheritsFrom(TACBrLibRespostaBase)) then
                  ListNode := xDoc.CreateElement(TACBrLibRespostaBase(ListItem).Sessao.Replace(' ', '_'))
                else
                  ListNode := xDoc.CreateElement(Propertie.Name);

                GravarXml(xDoc, ListNode, ListItem);
                ParentNode.AppendChild(ListNode);
              end;
            end
            else
            begin
              if (ClassObject.InheritsFrom(TACBrLibRespostaBase)) then
                ParentNode := xDoc.CreateElement(TACBrLibRespostaBase(ClassObject).Sessao.Replace(' ', '_'))
              else
                ParentNode := xDoc.CreateElement(Propertie.Name);

              GravarXml(xDoc, ParentNode, ClassObject);
            end;
          end;
        tkArray,
        tkDynArray:
          begin
            // Não é possivel ainda mexer com array
          end;
        tkSet:
           begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(GetSetProp(Target, Propertie.Name, True)));
          end;
        tkBool:
           begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(BoolToStr(AValue.AsBoolean)));
          end;
        tkEnumeration:
           begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(IntToStr(AValue.AsOrdinal)));
          end;
        tkInteger:
           begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(IntToStr(AValue.AsInteger)));
          end;
        tkInt64:
          begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(IntToStr(AValue.AsInt64)));
          end;
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            ParentNode.AppendChild(xDoc.CreateTextNode(Trim(AValue.AsString)));
          end;
        tkFloat:
          begin
            ParentNode := xDoc.CreateElement(Propertie.Name);
            FValue := AValue.AsExtended;

            if AValue.IsType<TDate>() then
            begin
              if (FValue > 0) then
                ParentNode.AppendChild(xDoc.CreateTextNode(DateToISO8601(FValue)));
            end
            else if AValue.IsType<TTime>() then
            begin
              if (FValue > 0) then
                ParentNode.AppendChild(xDoc.CreateTextNode(TimeToStr(FValue)));
            end
            else if AValue.IsType<TDateTime>() then
            begin
              if (FValue > 0) then
                ParentNode.AppendChild(xDoc.CreateTextNode(DateToISO8601(FValue)));
            end
            else
              ParentNode.AppendChild(xDoc.CreateTextNode(FloatToStr(FValue)));
          end;
      end;
      RootNode.AppendChild(ParentNode);
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

class function TACBrObjectSerializer.GerarIni(Item: TACBrLibRespostaBase): Ansistring;
var
  AIni: TMemIniFile;
begin
  AIni := TMemIniFile.Create('');

  try
    GravarIni(AIni, Item.Sessao, Item);
    AIni.ClearEmptySections;
    Result := AIni.AsString;
  finally
    if AIni <> nil then
      AIni.Free;
  end;
end;

class procedure TACBrObjectSerializer.GravarIni(const AIni: TCustomIniFile; const ASessao: String;
  const Target: TObject; const ASufix: string);
var
  PropList: TPropInfoList;
  i: Integer;
  FSessao, FSufix: String;
  ClassObject: TObject;
  CollectionObject: TCollection;
  CollectionItem: TCollectionItem;
  ListObject: TList;
  ListItem: TObject;
  FValue: Extended;
  Propertie: TRttiProperty;
  AValue, ARValue: TValue;
begin
  if Target = nil then Exit;
  if Target.ClassType = nil then Exit;

  PropList := TPropInfoList.Create(Target, tkProperties);

  try
    for Propertie in PropList.GetProperties do
    begin
      try
        if (not Propertie.IsReadable) then
        continue;

        AValue := Propertie.GetValue(Target);
        case AValue.Kind of
          tkClass:
            begin
              ClassObject := AValue.AsObject;
              if not Assigned(ClassObject) or (ClassObject = nil) then continue;

              if (ClassObject.InheritsFrom(TCollection)) then
              begin
                CollectionObject := TCollection(ClassObject);
                for i := 0 to CollectionObject.Count - 1 do
                begin
                  CollectionItem := CollectionObject.Items[i];

                  if ASufix.IsEmpty then
                    FSufix := String.Format(CSufixFormat, [i+1])
                  else
                    FSufix :=  String.Format(CSessionFormat, [ASufix, i+1]);

                  FSessao := Propertie.Name + FSufix;
                  GravarIni(AIni, FSessao, CollectionItem, FSufix);
                end;
              end
              else if (ClassObject.InheritsFrom(TList)) then
              begin
                ListObject := TList(ClassObject);
                for i := 0 to ListObject.Count - 1 do
                begin
                  ListItem := ListObject.Items[i];

                  if (ListItem.InheritsFrom(TACBrLibRespostaBase)) then
                  begin
                    FSessao := TACBrLibRespostaBase(ListItem).Sessao;
                    FSufix := '';
                  end
                  else
                  begin
                    if ASufix.IsEmpty then
                      FSufix := String.Format(CSufixFormat, [i+1])
                    else
                      FSufix :=  String.Format(CSessionFormat, [ASufix, i+1]);

                    FSessao := Propertie.Name + FSufix;
                  end;

                  GravarIni(AIni, FSessao, ListItem, FSufix);
                end;
              end
              else
              begin
                if (ClassObject.InheritsFrom(TACBrLibRespostaBase)) then
                  GravarIni(AIni, TACBrLibRespostaBase(ClassObject).Sessao, ClassObject)
                else
                  GravarIni(AIni, Propertie.Name, ClassObject);
              end;
            end;
          tkArray,
          tkDynArray:
            begin
              //Aparentemente ainda não funciona direito apesar de ter colocado um TObject da erro ao fazer cast.
              for i := 0 to Pred(AValue.GetArrayLength) do
              begin
                ARValue := AValue.GetArrayElement(i);
                ClassObject := ARValue.AsObject;
                if not Assigned(ClassObject) or (ClassObject = nil) then continue;

                if (ClassObject.InheritsFrom(TACBrLibRespostaBase)) then
                  GravarIni(AIni, TACBrLibRespostaBase(ClassObject).Sessao, ClassObject)
                else
                  GravarIni(AIni, Propertie.Name + IntToStr(i), ClassObject);
              end;
            end;
          tkSet:
              AIni.WriteStringLine(ASessao, Propertie.Name, GetSetProp(Target, Propertie.Name, True));
          tkBool:
            AIni.WriteBool(ASessao, Propertie.Name, AValue.AsBoolean);
          tkEnumeration:
            AIni.WriteInteger(ASessao, Propertie.Name, AValue.AsOrdinal);
          tkInteger:
            AIni.WriteInteger(ASessao, Propertie.Name, AValue.AsInteger);
          tkInt64:
            AIni.WriteInt64(ASessao, Propertie.Name, AValue.AsInt64);
          tkWString,
          tkUString,
          tkSString,
          tkLString,
          tkAString:
            AIni.WriteStringLine(ASessao, Propertie.Name, AValue.AsString);
          tkFloat:
            begin
              FValue := AValue.AsExtended;

              if AValue.IsType<TDate>() then
              begin
                if (FValue > 0) then
                  AIni.WriteDate(ASessao, Propertie.Name, FValue)
                else
                  AIni.WriteString(ASessao, Propertie.Name, '');
              end
              else if AValue.IsType<TTime>() then
              begin
                if (FValue > 0) then
                  AIni.WriteTime(ASessao, Propertie.Name, FValue)
                else
                  AIni.WriteString(ASessao, Propertie.Name, '');
              end
              else if AValue.IsType<TDateTime>() then
              begin
                if (FValue > 0) then
                  AIni.WriteDateTime(ASessao, Propertie.Name, FValue)
                else
                  AIni.WriteString(ASessao, Propertie.Name, '');
              end
              else
                AIni.WriteFloat(ASessao, Propertie.Name, FValue);
            end;
        end;
      finally
        Propertie.Free;
      end;
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

class function TACBrObjectSerializer.GerarJson(Item: TACBrLibRespostaBase): Ansistring;
var
  JSON: TJSONObject;
begin
  JSon := TJSONObject.Create;
  try
    GravarJson(JSon, Item.Sessao, Item);
    Result := JSON.AsJSON;
  finally
    if JSON <> nil then
      JSON.Free;
  end;
end;

class procedure TACBrObjectSerializer.GravarJson(const JSON: TJSONObject; const ASessao: String;
  const Target: TObject);
var
  PropList: TPropInfoList;
  i: Integer;
  JSONRoot: TJSONObject;
  ClassObject: TObject;
  CollectionObject: TCollection;
  CollectionItem: TCollectionItem;
  ListObject: TList;
  ListItem: TObject;
  FValue: Extended;
  Propertie: TRttiProperty;
  AValue: TValue;
begin
  if Target = nil then Exit;
  if Target.ClassType = nil then Exit;

  JSONRoot := TJSONObject.Create;
  JSON.Add(ASessao, JSONRoot);

  PropList := TPropInfoList.Create(Target, tkProperties);

  try
    for Propertie in PropList.GetProperties do
    begin
      AValue := Propertie.GetValue(Target);
      case AValue.Kind of
         tkClass:
          begin
            if not AValue.IsObject then continue;

            ClassObject := AValue.AsObject;
            if not Assigned(ClassObject) or (ClassObject = nil) then continue;

            if (ClassObject.InheritsFrom(TCollection)) then
            begin
              CollectionObject := TCollection(ClassObject);
              for i := 0 to CollectionObject.Count - 1 do
              begin
                CollectionItem := CollectionObject.Items[i];
                GravarJson(JSONRoot, Format(CSessionFormat, [Propertie.Name, i+1]), CollectionItem);
              end;
            end
            else if (ClassObject.InheritsFrom(TList)) then
            begin
              ListObject := TList(ClassObject);
              for i := 0 to ListObject.Count - 1 do
              begin
                ListItem := ListObject.Items[i];
                if (ListItem.ClassType.InheritsFrom(TACBrLibRespostaBase)) then
                  GravarJson(JSONRoot, TACBrLibRespostaBase(ListItem).Sessao, ListItem)
                else
                  GravarJson(JSONRoot, Format(CSessionFormat, [Propertie.Name, i+1]), ListItem);
              end;
            end
            else
            begin
              if (ClassObject.InheritsFrom(TACBrLibRespostaBase)) then
                GravarJson(JSONRoot, TACBrLibRespostaBase(ClassObject).Sessao, ClassObject)
              else
                GravarJson(JSONRoot, Propertie.Name, ClassObject);
            end;
          end;
        tkArray,
        tkDynArray:
          begin
            //Aparentemente ainda não funciona direito apesar de ter colocado um TObject da erro ao fazer cast.
          end;
        tkSet:
          JSONRoot.Add(Propertie.Name, GetSetProp(Target, Propertie.Name, True));
        tkBool:
          JSONRoot.Add(Propertie.Name, AValue.AsBoolean);
        tkEnumeration:
          JSONRoot.Add(Propertie.Name, AValue.AsOrdinal);
        tkInteger:
          JSONRoot.Add(Propertie.Name, AValue.AsInteger);
        tkInt64:
          JSONRoot.Add(Propertie.Name, AValue.AsInt64);
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          JSONRoot.Add(Propertie.Name, Trim(AValue.AsString));
        tkFloat:
          begin
            FValue := AValue.AsExtended;

            if AValue.IsType<TDate>() then
            begin
              if (FValue > 0) then
                JSONRoot.Add(Propertie.Name, DateToISO8601(FValue))
              else
                JSONRoot.Add(Propertie.Name, '');
            end
            else if AValue.IsType<TTime>() then
            begin
              if (FValue > 0) then
                JSONRoot.Add(Propertie.Name, TimeToStr(FValue))
              else
                JSONRoot.Add(Propertie.Name, '');
            end
            else if AValue.IsType<TDateTime>() then
            begin
              if (FValue > 0) then
                JSONRoot.Add(Propertie.Name, DateToISO8601(FValue))
              else
                JSONRoot.Add(Propertie.Name, '');
            end
            else
              JSONRoot.Add(Propertie.Name, FValue);
          end;
      end;
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

end.
