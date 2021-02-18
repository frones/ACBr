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

unit ACBrLibResposta;

interface

uses
  SysUtils, Classes, laz2_DOM, laz2_XMLWrite,
  inifiles, fpjson, jsonparser, TypInfo,
  rttiutils, ACBrBase, ACBrUtil;

const
  CSessaoHttpResposta = 'RespostaHttp';
  CSessionFormat = '%s%.3d';
  CSufixFormat = '%.3d';

type
  TACBrLibRespostaTipo = (resINI, resXML, resJSON);
  TACBrLibCodificacao = (codUTF8, codANSI);

  { TACBrLibRespostaBase }
  TACBrLibRespostaBase = class abstract
  private
    FSessao: String;
    FTipo: TACBrLibRespostaTipo;
    FFormato: TACBrLibCodificacao;

    function GerarXml: Ansistring;
    function GerarIni: Ansistring;
    function GerarJson: Ansistring;

  protected
    procedure GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode; const Target: TObject); virtual;
    procedure GravarIni(const AIni: TCustomIniFile; const ASessao: String; const Target: TObject; const ASufix: string = ''); virtual;
    procedure GravarJson(const JSON: TJSONObject; const ASessao: String; const Target: TObject); virtual;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);

    property Sessao: String read FSessao;
    property Tipo: TACBrLibRespostaTipo read FTipo;
    property Formato: TACBrLibCodificacao read FFormato;

    function Gerar: Ansistring; virtual;

  end;

  { TACBrLibResposta }
  TACBrLibResposta<T: TACBrComponent> = class abstract(TACBrLibRespostaBase)
  public
    procedure Processar(const Control: T); virtual; abstract;
  end;

  { TACBrLibHttpResposta }
  TACBrLibHttpResposta = class(TACBrLibRespostaBase)
  private
    FWebService: string;
    FCodigoHTTP: Integer;
    FMsg: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property WebService: String read FWebService write FWebService;
    property CodigoHTTP: Integer read FCodigoHTTP write FCodigoHTTP;
    property Msg: string read FMsg write FMsg;

  end;

  { TLibImpressaoResposta }
  TLibImpressaoResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;

  public
    constructor Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Msg: string read FMsg write FMsg;

  end;

implementation

uses
  ACBrRtti, ACBrLibHelpers;

{ TACBrLibResposta }
constructor TACBrLibRespostaBase.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create;
  FSessao := ASessao;
  FTipo := ATipo;
  FFormato := AFormato;
end;

function TACBrLibRespostaBase.Gerar: Ansistring;
begin
  case FTipo of
    resXML: Result := GerarXml;
    resJSON: Result := GerarJson;
    else
      Result := GerarIni;
  end;

  if FFormato = codANSI then
    Result := ACBrUTF8ToAnsi(Result);
end;

function TACBrLibRespostaBase.GerarXml: Ansistring;
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

procedure TACBrLibRespostaBase.GravarXml(const xDoc: TXMLDocument; const RootNode: TDomNode; const Target: TObject);
Var
  PropList: TPropInfoList;
  i: Integer;
  ParentNode: TDomNode;
  ClassObject: TObject;
  CollectionObject: TCollection;
  CollectionItem: TCollectionItem;
  ListObject: TList;
  ListItem: TObject;
  FValue: Extended;
  Propertie: TRttiProperty;
  AValue, ARValue: TValue;
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
              CollectionObject := TCollection(ClassObject);
              for i := 0 to CollectionObject.Count - 1 do
              begin
                CollectionItem := CollectionObject.Items[i];
                GravarXml(xDoc, RootNode, CollectionItem);
              end;
            end
            else if (ClassObject.InheritsFrom(TList)) then
            begin
              ListObject := TList(ClassObject);
              for i := 0 to ListObject.Count - 1 do
              begin
                ListItem := ListObject.Items[i];
                GravarXml(xDoc, RootNode, ListItem);
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

            if (AValue.IsType<TDate>()) then
              ParentNode.AppendChild(xDoc.CreateTextNode(DateToStr(FValue)))
            else if (AValue.IsType<TTime>()) then
              ParentNode.AppendChild(xDoc.CreateTextNode(TimeToStr(FValue)))
            else if(AValue.IsType<TDateTime>())then
              ParentNode.AppendChild(xDoc.CreateTextNode(DateTimeToStr(FValue)))
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

function TACBrLibRespostaBase.GerarIni: Ansistring;
var
  AIni: TMemIniFile;
begin
  AIni := TMemIniFile.Create('');

  try
    GravarIni(AIni, Sessao, Self);
    Result := AIni.AsString;
  finally
    if AIni <> nil then
      AIni.Free;
  end;
end;

procedure TACBrLibRespostaBase.GravarIni(const AIni: TCustomIniFile; const ASessao: String; const Target: TObject; const ASufix: string);
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
      if not Propertie.IsReadable then continue;

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
          end;
        tkSet:
            AIni.WriteStringLine(ASessao, Propertie.Name, GetSetProp(Target, Propertie.Name, True));
        tkBool:
          AIni.WriteBool(ASessao, Propertie.Name, AValue.AsBoolean);
        tkEnumeration:
          AIni.WriteInt64(ASessao, Propertie.Name, AValue.AsOrdinal);
        tkInteger:
          AIni.WriteInt64(ASessao, Propertie.Name, AValue.AsInteger);
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

            if (AValue.IsType<TDate>()) then
              AIni.WriteDate(ASessao, Propertie.Name, FValue)
            else if (AValue.IsType<TTime>()) then
              AIni.WriteTime(ASessao, Propertie.Name, FValue)
            else if (AValue.IsType<TDateTime>()) then
              AIni.WriteDateTime(ASessao, Propertie.Name, FValue)
            else
              AIni.WriteFloat(ASessao, Propertie.Name, FValue);
          end;
      end;
    end;
  finally
    if PropList <> nil then
      PropList.Free;
  end;
end;

function TACBrLibRespostaBase.GerarJson: Ansistring;
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

procedure TACBrLibRespostaBase.GravarJson(const JSON: TJSONObject; const ASessao: String; const Target: TObject);
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
  AValue, ARValue: TValue;
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
                GravarJson(JSONRoot, Format(CSessionFormat, [Propertie.Name, i+1]), CollectionItem)
              end;
            end
            else if (ClassObject.InheritsFrom(TList)) then
            begin
              ListObject := TList(ClassObject);
              for i := 0 to ListObject.Count - 1 do
              begin
                ListItem := ListObject.Items[i];
                GravarJson(JSONRoot, Format(CSessionFormat, [Propertie.Name, i+1]), ListItem)
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
          JSONRoot.Add(Propertie.Name, StringToJSONString(Trim(AValue.AsString), False));
        tkFloat:
          begin
            FValue := AValue.AsExtended;

            if (AValue.IsType<TDate>()) then
              JSONRoot.Add(Propertie.Name, DateToStr(FValue))
            else if (AValue.IsType<TTime>()) then
              JSONRoot.Add(Propertie.Name, TimeToStr(FValue))
            else if (AValue.IsType<TDateTime>()) then
              JSONRoot.Add(Propertie.Name, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', FValue))
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

{ TACBrLibHttpResposta }
constructor TACBrLibHttpResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoHttpResposta, ATipo, AFormato);
end;

{ TLibImpressaoResposta }
constructor TLibImpressaoResposta.Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('Impressao', ATipo, AFormato);
  Msg := Format('%d Documento (s) impresso(s) com sucesso', [QtdImpresso]);
end;

end.


