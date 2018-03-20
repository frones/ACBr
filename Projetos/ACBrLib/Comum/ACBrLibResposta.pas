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
  SysUtils, Classes, laz2_DOM, laz2_XMLWrite,
  inifiles, fpjson, jsonparser, TypInfo, rttiutils;

type

  { TACBrLibResposta }
  TACBrLibRespostaTipo = (resINI, resXML, resJSON);

  TACBrLibResposta = class
  private
    FSessao: String;
    FTipo: TACBrLibRespostaTipo;
  protected
    function GerarXml: String;
    function GerarIni: String;
    function GerarJson: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);

    property Sessao: String read FSessao;

    function Gerar: String;
  end;

implementation

constructor TACBrLibResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create;
  FSessao := ASessao;
  FTipo := ATipo;
end;

function TACBrLibResposta.GerarXml: String;
var
  PropList: TPropInfoList;
  i: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  xDoc: TXMLDocument;
  RootNode, ParentNode, Node: TDomNode;
  Stream: TMemoryStream;
begin
  xDoc := TXMLDocument.Create;
  PropList := TPropInfoList.Create(Self, tkProperties);

  try
    RootNode := xDoc.CreateElement(Sessao);
    xDoc.AppendChild(RootNode);
    RootNode := xDoc.DocumentElement;

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
          Node := xDoc.CreateTextNode(IntToStr(GetOrdProp(Self, PI)));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          Node := xDoc.CreateTextNode(Trim(GetStrProp(Self, PI)));
        tkFloat:
          if (PT = TypeInfo(TDateTime)) then
            Node := xDoc.CreateTextNode(DateTimeToStr(GetFloatProp(Self, PI)))
          else
            Node := xDoc.CreateTextNode(FloatToStr(GetFloatProp(Self, PI)));
      end;
      ParentNode.AppendChild(Node);
      RootNode.AppendChild(ParentNode);
    end;

    Stream := TMemoryStream.Create();
    WriteXML(xDoc.FirstChild, Stream);
    SetString(Result, PChar(Stream.Memory), Stream.Size div SizeOf(char));
  finally
    if Stream <> nil then
      Stream.Free;
    if PropList <> nil then
      PropList.Free;
    if xDoc <> nil then
      xDoc.Free;
  end;
end;

function TACBrLibResposta.GerarIni: String;
var
  PropList: TPropInfoList;
  i: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  AIni: TMemIniFile;
  TList: TStringList;
begin
  AIni := TMemIniFile.Create('');
  TList := TStringList.Create;
  PropList := TPropInfoList.Create(Self, tkProperties);

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
          AIni.WriteInt64(Sessao, PI^.Name, GetOrdProp(Self, PI));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          AIni.WriteString(Sessao, PI^.Name, Trim(GetStrProp(Self, PI)));
        tkFloat:
          if (PT = TypeInfo(TDateTime)) then
            AIni.WriteDateTime(Sessao, PI^.Name, GetFloatProp(Self, PI))
          else
            AIni.WriteFloat(Sessao, PI^.Name, GetFloatProp(Self, PI));
      end;
    end;

    AIni.GetStrings(TList);
    Result := TList.Text;
  finally
    if PropList <> nil then
      PropList.Free;
    if TList <> nil then
      TList.Free;
    if AIni <> nil then
      AIni.Free;
  end;
end;

function TACBrLibResposta.GerarJson: String;
var
  PropList: TPropInfoList;
  i: Integer;
  PI: PPropInfo;
  PT: PTypeInfo;
  JSON, JSONRoot: TJSONObject;
  Aux: Double;
begin
  JSon := TJSONObject.Create;
  JSONRoot := TJSONObject.Create;
  JSON.Add(Sessao, JSONRoot);

  PropList := TPropInfoList.Create(Self, tkProperties);

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
          JSONRoot.Add(PI^.Name, GetOrdProp(Self, PI));
        tkWString,
        tkUString,
        tkSString,
        tkLString,
        tkAString:
          JSONRoot.Add(PI^.Name, Trim(GetStrProp(Self, PI)));
        tkFloat:
          if (PT = TypeInfo(TDateTime)) then
          begin
            Aux := GetFloatProp(Self, PI);
            JSONRoot.Add(Sessao, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', TDateTime(Aux)));
          end
          else
            JSONRoot.Add(Sessao, GetFloatProp(Self, PI));
      end;
    end;

    Result := JSON.AsJSON;
  finally
    if PropList <> nil then
      PropList.Free;
    if JSON <> nil then
      JSON.Free;
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


