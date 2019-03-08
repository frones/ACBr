{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}

unit ACBreSocialEventos;

interface

uses
  SysUtils, Classes, synautil,
  pcesIniciais, pcesTabelas, pcesNaoPeriodicos, pcesPeriodicos,
  pcesConversaoeSocial;

type
  TEventos = class;
  TGeradosCollection = class;
  TGeradosCollectionItem = class;

  TGeradosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TGeradosCollectionItem;
    procedure SetItem(Index: Integer; Value: TGeradosCollectionItem);
  public
    constructor create(AOwner: TEventos);
    function Add: TGeradosCollectionItem;
    property Items[Index: Integer]: TGeradosCollectionItem read GetItem write SetItem; default;
  end;

  TGeradosCollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FPathNome: String;
    FXML: String;
    FidEvento: String;
  public
    property TipoEvento: TTipoEvento read FTipoEvento write FTipoEvento;
    property PathNome: String read FPathNome write FPathNome;
    property idEvento: String read FidEvento write FidEvento;
    property XML: String read FXML write FXML;
  end;

  TEventos = class(TComponent)
  private
    FIniciais: TIniciais;
    FTabelas: TTabelas;
    FNaoPeriodicos: TNaoPeriodicos;
    FPeriodicos: TPeriodicos;
    FTipoEmpregador: TEmpregador;
    FGerados: TGeradosCollection;

    procedure SetIniciais(const Value: TIniciais);
    procedure SetNaoPeriodicos(const Value: TNaoPeriodicos);
    procedure SetPeriodicos(const Value: TPeriodicos);
    procedure SetTabelas(const Value: TTabelas);
    function GetCount: integer;
    procedure SetGerados(const Value: TGeradosCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;//verificar se será necessário, se TIniciais for TComponent;

    procedure GerarXMLs;
    procedure SaveToFiles;
    procedure Clear;

    function LoadFromFile(const CaminhoArquivo: String; ArqXML: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(AXMLString: String): Boolean;
    function LoadFromStringINI(const AINIString: String): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    property Count:          Integer        read GetCount;
    property Iniciais:       TIniciais      read FIniciais       write SetIniciais;
    property Tabelas:        TTabelas       read FTabelas        write SetTabelas;
    property NaoPeriodicos:  TNaoPeriodicos read FNaoPeriodicos  write SetNaoPeriodicos;
    property Periodicos:     TPeriodicos    read FPeriodicos     write SetPeriodicos;
    property TipoEmpregador: TEmpregador    read FTipoEmpregador write FTipoEmpregador;
    property Gerados: TGeradosCollection    read FGerados        write SetGerados;
  end;

implementation

uses
  dateutils,
  ACBrUtil, ACBrDFeUtil, ACBreSocial;

{ TGeradosCollection }

function TGeradosCollection.Add: TGeradosCollectionItem;
begin
  Result := TGeradosCollectionItem(inherited add());
//  Result.Create;
end;

constructor TGeradosCollection.create(AOwner: TEventos);
begin
  Inherited create(TGeradosCollectionItem);
end;

function TGeradosCollection.GetItem(Index: Integer): TGeradosCollectionItem;
begin
  Result := TGeradosCollectionItem(inherited GetItem(Index));
end;

procedure TGeradosCollection.SetItem(Index: Integer;
  Value: TGeradosCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEventos }

procedure TEventos.Clear;
begin
  FIniciais.Clear;
  FTabelas.Clear;
  FNaoPeriodicos.Clear;
  FPeriodicos.Clear;
  FGerados.Clear;
end;

constructor TEventos.Create(AOwner: TComponent);
begin
  inherited;

  FIniciais := TIniciais.Create(AOwner);
  FTabelas := TTabelas.Create(AOwner);
  FNaoPeriodicos := TNaoPeriodicos.Create(AOwner);
  FPeriodicos := TPeriodicos.Create(AOwner);
  FGerados := TGeradosCollection.create(Self);
end;

destructor TEventos.Destroy;
begin
  FIniciais.Free;
  FTabelas.Free;
  FNaoPeriodicos.Free;
  FPeriodicos.Free;
  FGerados.Free;
  
  inherited;
end;

procedure TEventos.GerarXMLs;
begin
  FTipoEmpregador := TACBreSocial(Self.Owner).Configuracoes.Geral.TipoEmpregador;

  Self.Iniciais.GerarXMLs;
  Self.Tabelas.GerarXMLs;
  Self.NaoPeriodicos.GerarXMLs;
  Self.Periodicos.GerarXMLs;
end;

function TEventos.GetCount: integer;
begin
  Result := Self.Iniciais.Count +
            Self.Tabelas.Count +
            Self.NaoPeriodicos.Count +
            Self.Periodicos.Count;
end;

procedure TEventos.SaveToFiles;
begin
  // Limpa a lista para não ocorrer duplicidades.
  Gerados.Clear;

  Self.Iniciais.SaveToFiles;
  Self.Tabelas.SaveToFiles;
  Self.NaoPeriodicos.SaveToFiles;
  Self.Periodicos.SaveToFiles;
end;

procedure TEventos.SetGerados(const Value: TGeradosCollection);
begin
  FGerados := Value;
end;

procedure TEventos.SetIniciais(const Value: TIniciais);
begin
  FIniciais.Assign(Value);
end;

procedure TEventos.SetTabelas(const Value: TTabelas);
begin
  FTabelas.Assign(Value);
end;

procedure TEventos.SetNaoPeriodicos(const Value: TNaoPeriodicos);
begin
  FNaoPeriodicos.Assign(Value);
end;

procedure TEventos.SetPeriodicos(const Value: TPeriodicos);
begin
  FPeriodicos.Assign(Value);
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

function TEventos.LoadFromString(AXMLString: String): Boolean;
var
//  AXML: AnsiString;
  AXML: String;
  P: integer;

  function PoseSocial: integer;
  begin
    Result := pos('</eSocial>', AXMLString);
  end;

begin
  Result := False;
  P := PoseSocial;

  while P > 0 do
  begin
    AXML := copy(AXMLString, 1, P + 9);
    AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));

    Result := Self.Iniciais.LoadFromString(AXML);
    Result := Self.Tabelas.LoadFromString(AXML) or Result;
    Result := Self.NaoPeriodicos.LoadFromString(AXML) or Result;
    Result := Self.Periodicos.LoadFromString(AXML) or Result;

    SaveToFiles;

    P := PoseSocial;
  end;
end;

function TEventos.LoadFromStringINI(const AINIString: String): Boolean;
begin
  Result := Self.Iniciais.LoadFromIni(AIniString);
  Result := Self.Tabelas.LoadFromIni(AIniString) or Result;
  Result := Self.NaoPeriodicos.LoadFromIni(AIniString) or Result;
  Result := Self.Periodicos.LoadFromIni(AIniString) or Result;

  SaveToFiles;
end;

function TEventos.LoadFromIni(const AIniString: String): Boolean;
begin
  // O valor False no segundo parâmetro indica que o conteudo do arquivo não é
  // um XML.
  Result := LoadFromFile(AIniString, False);
end;

end.
