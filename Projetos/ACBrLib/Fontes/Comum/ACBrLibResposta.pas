{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibResposta;

interface

uses
  SysUtils, Classes, laz2_DOM, laz2_XMLWrite, StrUtils,
  inifiles, fpjson, jsonparser, TypInfo, rttiutils,
  ACBrUtil;

type
  TACBrCollection = class abstract (TCollection)
  private
    FSessao: String;
    FSessaoItem: String;

  public
    constructor Create(AItemClass: TCollectionItemClass; const ASessao: String; const ASessaoItem: String = ''); reintroduce;

    property Sessao: String read FSessao;
    property SessaoItem: String read FSessaoItem;

  end;

  { TACBrLibResposta }
  TACBrLibRespostaTipo = (resINI, resXML, resJSON);

  TACBrLibResposta = class abstract
  private
    FTipo: TACBrLibRespostaTipo;

  protected
    FSessao: String;

    function GerarXml: String;
    function GerarIni: String;
    function GerarJson: String;

    procedure GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode; const Target: TObject); virtual;
    procedure GravarIni(const AIni: TCustomIniFile; const ASessao: String; const Target: TObject; IsCollection: Boolean = false); virtual;
    procedure GravarJson(const JSON: TJSONObject; const ASessao: String; const Target: TObject); virtual;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);

    property Sessao: String read FSessao;

    function Gerar: String;
  end;

implementation

{ TACBrCollection }
constructor TACBrCollection.Create(AItemClass: TCollectionItemClass; const ASessao: String; const ASessaoItem: String);
begin
  inherited Create(AItemClass);

  FSessao := ASessao;
  FSessaoItem := ASessaoItem;
end;

{ TACBrLibResposta }

constructor TACBrLibResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create;
  FSessao := ASessao;
  FTipo := ATipo;
end;

function TACBrLibResposta.GerarXml: String;
var
  xDoc: TXMLDocument;
  RootNode: TDomNode;
  Stream: TMemoryStream;
begin
  xDoc := TXMLDocument.Create;

  try
    RootNode := xDoc.CreateElement(Sessao);
    xDoc.AppendChild(RootNode);
    RootNode := xDoc.DocumentElement;
    GravarXml(xDoc, RootNode, Self);
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

procedure TACBrLibResposta.GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode; const Target: TObject);
Var
  PropList: TPropInfoList;
  i: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  ParentNode, Node: TDomNode;
begin
  PropList := TPropInfoList.Create(Target, tkProperties);

  try
    for i := 0 to PropList.Count - 1 do
    begin
      PI := PropList.Items[i];
      PT := PI^.PropType;
      ParentNode := xDoc.CreateElement(Pi^.Name);
      case PT^.Kind of
        tkSet,
        tkEnumeration,
        tkInteger,
        tkBool,
        tkInt64:
          Node := xDoc.CreateTextNode(IntToStr(GetOrdProp(Target, PI)));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          Node := xDoc.CreateTextNode(Trim(GetStrProp(Target, PI)));
        tkFloat:
          if (PT = TypeInfo(TDateTime)) then
            Node := xDoc.CreateTextNode(DateTimeToStr(GetFloatProp(Target, PI)))
          else
            Node := xDoc.CreateTextNode(FloatToStr(GetFloatProp(Target, PI)));
      end;
      ParentNode.AppendChild(Node);
      RootNode.AppendChild(ParentNode);
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

function TACBrLibResposta.GerarIni: String;
var
  AIni: TMemIniFile;
  TList: TStringList;
begin
  AIni := TMemIniFile.Create('');
  TList := TStringList.Create;

  try
    GravarIni(AIni, Sessao, Self);

    AIni.GetStrings(TList);
    Result := TList.Text;
  finally
    if TList <> nil then
      TList.Free;
    if AIni <> nil then
      AIni.Free;
  end;
end;

procedure TACBrLibResposta.GravarIni(const AIni: TCustomIniFile; const ASessao: String; const Target: TObject; IsCollection: Boolean);
var
  PropList: TPropInfoList;
  i, j: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  SetValues: TSplitResult;
  SetValue, Sessao: String;
  SetOrdValue: Int64;
  ClassObject: TObject;
  CollectionObject: TCollection;
  CollectionItem: TCollectionItem;
begin
  PropList := TPropInfoList.Create(Target, tkProperties);

  try
    for i := 0 to PropList.Count - 1 do
    begin
      PI := PropList.Items[i];
      PT := PI^.PropType;
      case PT^.Kind of
        tkClass:
          begin
            ClassObject := GetObjectProp(Target, PI);
            if ClassObject = nil then continue;

            if (ClassObject.InheritsFrom(TCollection)) then
            begin
              CollectionObject := TCollection(ClassObject);
              for j := 0 to CollectionObject.Count - 1 do
              begin
                CollectionItem := CollectionObject.Items[j];
                Sessao := IfThen(IsCollection, ASessao + PI.Name, PI.Name);
                GravarIni(AIni, Sessao + FormatFloat('000', J+1), CollectionItem, True)
              end;
            end
            else
            begin
              if (ClassObject.InheritsFrom(TACBrLibResposta)) then
              begin
                Sessao := IfThen(IsCollection, ASessao + TACBrLibResposta(ClassObject).Sessao, TACBrLibResposta(ClassObject).Sessao);
                GravarIni(AIni, Sessao, ClassObject, IsCollection)
              end
              else
              begin
                Sessao := IfThen(IsCollection, ASessao + PI.Name, PI.Name);
                GravarIni(AIni, PI.Name, ClassObject, IsCollection);
              end;
            end;
          end;
        tkSet:
          begin
            SetOrdValue := 0;
            SetValues := Split(',', GetSetProp(Target, PI, false));
            for j := 0 to Length(SetValues) - 1 do
            begin
              SetOrdValue := SetOrdValue + (1 << GetEnumValue(PT, SetValue));
            end;

            AIni.WriteInt64(ASessao, PI^.Name, SetOrdValue);
          end;
        tkBool,
        tkEnumeration,
        tkInteger,
        tkInt64:
          AIni.WriteInt64(ASessao, PI^.Name, GetOrdProp(Target, PI));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          AIni.WriteString(ASessao, PI^.Name, Trim(GetStrProp(Target, PI)));
        tkFloat:
          begin
            if (PT = TypeInfo(TDateTime)) then
              AIni.WriteDateTime(ASessao, PI^.Name, GetFloatProp(Target, PI))
            else
              AIni.WriteFloat(ASessao, PI^.Name, GetFloatProp(Target, PI));
          end;
      end;
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

function TACBrLibResposta.GerarJson: String;
var
  JSON: TJSONObject;
begin
  JSon := TJSONObject.Create;
  try
    GravarJson(JSon, Sessao, Self);
    Result := JSON.AsJSON;
  finally
    if JSON <> nil then
      JSON.Free;
  end;
end;

procedure TACBrLibResposta.GravarJson(const JSON: TJSONObject; const ASessao: String; const Target: TObject);
var
  PropList: TPropInfoList;
  i: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  Aux: Double;
  JSONRoot: TJSONObject;
begin
  JSONRoot := TJSONObject.Create;
  JSON.Add(ASessao, JSONRoot);
  PropList := TPropInfoList.Create(Target, tkProperties);

  try
    for i := 0 to PropList.Count - 1 do
    begin
      PI := PropList.Items[i];
      PT := PI^.PropType;
      case PT^.Kind of
        tkSet,
        tkEnumeration,
        tkInteger,
        tkBool,
        tkInt64:
          JSONRoot.Add(PI^.Name, GetOrdProp(Target, PI));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          JSONRoot.Add(PI^.Name, Trim(GetStrProp(Target, PI)));
        tkFloat:
          if (PT = TypeInfo(TDateTime)) then
          begin
            Aux := GetFloatProp(Target, PI);
            JSONRoot.Add(PI^.Name, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', TDateTime(Aux)));
          end
          else
            JSONRoot.Add(PI^.Name, GetFloatProp(Target, PI));
      end;
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

function TACBrLibResposta.Gerar: String;
begin
  case FTipo of
    resXML: Result := GerarXml;
    resJSON: Result := GerarJson;
    else
      Result := GerarIni;
  end;
end;

end.


