{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit ACBrReinfEventos;

interface

uses
  SysUtils, Classes, synautil,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnEventosReinf, pcnConversaoReinf;

type
  TEventos = class;
  TGeradosCollection = class;
  TGeradosCollectionItem = class;

  TGeradosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TGeradosCollectionItem;
    procedure SetItem(Index: Integer; Value: TGeradosCollectionItem);
  public
    function Add: TGeradosCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TGeradosCollectionItem;

    property Items[Index: Integer]: TGeradosCollectionItem read GetItem write SetItem; default;
  end;

  TGeradosCollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FPathNome: String;
    FXML: String;
    FIdEvento: String;
  public
    property TipoEvento: TTipoEvento read FTipoEvento write FTipoEvento;
    property IdEvento: String read FIdEvento write FIdEvento;
    property PathNome: String read FPathNome write FPathNome;
    property XML: String read FXML write FXML;
  end;

  TEventos = class(TComponent)
  private
    FReinfEventos: TReinfEventos;
    FTipoContribuinte: TContribuinte;
    FGerados: TGeradosCollection;

    procedure SetReinfEventos(const Value: TReinfEventos);
    function GetCount: integer;
    procedure SetGerados(const Value: TGeradosCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;//verificar se será necessário, se TReinfEventos for TComponent;

    procedure Gerar;
    procedure Assinar;
    procedure Validar;
    procedure GerarXMLs;
    procedure SaveToFiles;
    procedure Clear;

    function LoadFromFile(const CaminhoArquivo: String; ArqXML: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(const AXMLString: String): Boolean;
    function LoadFromStringINI(const AINIString: String): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    property Count:        Integer           read GetCount;
    property ReinfEventos: TReinfEventos     read FReinfEventos     write SetReinfEventos;
    property TipoContribuinte: TContribuinte read FTipoContribuinte write FTipoContribuinte;
    property Gerados: TGeradosCollection     read FGerados          write SetGerados;
  end;

implementation

uses
  dateutils,
  ACBrUtil, ACBrDFeUtil, ACBrReinf;

{ TGeradosCollection }

function TGeradosCollection.Add: TGeradosCollectionItem;
begin
  Result := Self.New;
end;

function TGeradosCollection.GetItem(Index: Integer): TGeradosCollectionItem;
begin
  Result := TGeradosCollectionItem(inherited Items[Index]);
end;

function TGeradosCollection.New: TGeradosCollectionItem;
begin
  Result := TGeradosCollectionItem.Create;
  Self.Add(Result);
end;

procedure TGeradosCollection.SetItem(Index: Integer;
  Value: TGeradosCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TEventos }

procedure TEventos.Clear;
begin
  FReinfEventos.Clear;
  FGerados.Clear;
end;

constructor TEventos.Create(AOwner: TComponent);
begin
  inherited;

  FReinfEventos := TReinfEventos.Create(AOwner);
  FGerados := TGeradosCollection.Create;
end;

destructor TEventos.Destroy;
begin
  FReinfEventos.Free;
  FGerados.Free;

  inherited;
end;

procedure TEventos.Gerar;
begin
  FTipoContribuinte := TACBrReinf(Self.Owner).Configuracoes.Geral.TipoContribuinte;

  Self.ReinfEventos.Gerar;
end;

procedure TEventos.Assinar;
begin
  FTipoContribuinte := TACBrReinf(Self.Owner).Configuracoes.Geral.TipoContribuinte;

  Self.ReinfEventos.Assinar;
end;

procedure TEventos.Validar;
begin
  FTipoContribuinte := TACBrReinf(Self.Owner).Configuracoes.Geral.TipoContribuinte;

  Self.ReinfEventos.Validar;
end;

procedure TEventos.GerarXMLs;
begin
  Gerar;
  Assinar;
  Validar;
end;

function TEventos.GetCount: integer;
begin
  Result := Self.ReinfEventos.Count;
end;

procedure TEventos.SaveToFiles;
begin
  // Limpa a lista para não ocorrer duplicidades.
  Gerados.Clear;

  Self.ReinfEventos.SaveToFiles;
end;

procedure TEventos.SetGerados(const Value: TGeradosCollection);
begin
  FGerados := Value;
end;

procedure TEventos.SetReinfEventos(const Value: TReinfEventos);
begin
  FReinfEventos.Assign(Value);
end;

function TEventos.LoadFromFile(const CaminhoArquivo: String; ArqXML: Boolean = True): Boolean;
var
  ArquivoXML: TStringList;
  XML: String;
  XMLOriginal: AnsiString;
begin
  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;
  finally
    ArquivoXML.Free;
  end;

  // Converte de UTF8 para a String nativa da IDE //
  XML := DecodeToString(XMLOriginal, True);

  if ArqXML then
    Result := LoadFromString(XML)
  else
    Result := LoadFromStringINI(XML);
end;

function TEventos.LoadFromStream(AStream: TStringStream): Boolean;
var
  XMLOriginal: AnsiString;
begin
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal));
end;

function TEventos.LoadFromString(const AXMLString: String): Boolean;
var
  AXML, AXMLStr: String;
  P: integer;

  function PosReinf: integer;
  begin
    Result := pos('</Reinf>', AXMLStr);
  end;

begin
  Result := False;
  AXMLStr := AXMLString;
  P := PosReinf;

  while P > 0 do
  begin
    AXML := copy(AXMLStr, 1, P + 7);
    AXMLStr := Trim(copy(AXMLStr, P + 8, length(AXMLStr)));

    Result := Self.ReinfEventos.LoadFromString(AXML);
    SaveToFiles;

    P := PosReinf;
  end;
end;

function TEventos.LoadFromStringINI(const AINIString: String): Boolean;
begin
  Result := Self.ReinfEventos.LoadFromIni(AIniString);
  SaveToFiles;
end;

function TEventos.LoadFromIni(const AIniString: String): Boolean;
begin
  // O valor False no segundo parâmetro indica que o conteudo do arquivo não é
  // um XML.
  Result := LoadFromFile(AIniString, False);
end;

end.
