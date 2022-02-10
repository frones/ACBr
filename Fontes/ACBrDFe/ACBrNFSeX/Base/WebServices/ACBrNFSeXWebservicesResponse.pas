{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Dias                                     }
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

unit ACBrNFSeXWebservicesResponse;

interface

uses
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrNFSeXClass, ACBrNFSeXConversao, ACBrNFSeXWebserviceBase;

type
  TNFSeEventoCollectionItem = class
  private
    FCodigo: String;
    FDescricao: String;
    FCorrecao: String;
  public
    property Codigo: String read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Correcao: String read FCorrecao write FCorrecao;
  end;

  TNFSeEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNFSeEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TNFSeEventoCollectionItem);
  public
    function New: TNFSeEventoCollectionItem;
    function Add(ANota: TNFSeEventoCollectionItem): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TNFSeEventoCollectionItem); reintroduce;

    property Items[Index: Integer]: TNFSeEventoCollectionItem read GetItem write SetItem; default;
  end;

  TNotasCanceladasCollectionItem = class
  private
    FNumeroNota: String;
    FCodigoVerficacao: String;
    FInscricaoMunicipalPrestador: String;
  public
    property NumeroNota: String read FNumeroNota write FNumeroNota;
    property CodigoVerficacao: String read FCodigoVerficacao write FCodigoVerficacao;
    property InscricaoMunicipalPrestador: String read FInscricaoMunicipalPrestador write FInscricaoMunicipalPrestador;
  end;

  TNotasCanceladasCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNotasCanceladasCollectionItem;
    procedure SetItem(Index: Integer; Value: TNotasCanceladasCollectionItem);
  public
    function New: TNotasCanceladasCollectionItem; reintroduce;
    function Add(ANota: TNotasCanceladasCollectionItem): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TNotasCanceladasCollectionItem); reintroduce;

    property Items[Index: Integer]: TNotasCanceladasCollectionItem read GetItem write SetItem; default;
  end;

  TRetCancelamento = class
  private
    FNumeroLote: String;
    FSituacao: String;
    FDataHora: TDateTime;
    FMsgCanc: String;
    FSucesso: String;
    FLink: String;
    FNumeroNota: string;
    FPedido: TPedidocancelamento;
    FNotasCanceladas: TNotasCanceladasCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Situacao: String read FSituacao write FSituacao;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property MsgCanc: String read FMsgCanc write FMsgCanc;
    property Sucesso: String read FSucesso write FSucesso;
    property Link: String read FLink write FLink;
    property NumeroNota: string read FNumeroNota  write FNumeroNota;
    property Pedido: TPedidocancelamento read FPedido;
    property NotasCanceladas: TNotasCanceladasCollection read FNotasCanceladas;
  end;

  TNFSeParamsResponse = class
  private
    FXml: String;
    FTagEnvio: String;
    FPrefixo: String;
    FPrefixo2: String;
    FNameSpace: String;
    FNameSpace2: String;
    FIdAttr: String;
    FVersao: String;
    FSerie: String;
    FMotivo: String;
    FCodVerif: String;
  public
    procedure Clear;

    property Xml: String read FXml write FXml;
    property TagEnvio: String read FTagEnvio write FTagEnvio;
    property Prefixo: String read FPrefixo write FPrefixo;
    property Prefixo2: String read FPrefixo2 write FPrefixo2;
    property NameSpace: String read FNameSpace write FNameSpace;
    property NameSpace2: String read FNameSpace2 write FNameSpace2;
    property IdAttr: String read FIdAttr write FIdAttr;
    property Versao: String read FVersao write FVersao;
    property Serie: String read FSerie write FSerie;
    property Motivo: String read FMotivo write FMotivo;
    property CodVerif: String read FCodVerif write FCodVerif;
  end;

  TNFSeWebserviceResponse = class
  private
    FSituacao: String;
    FDescSituacao: String;
    FLote: string;
    FSucesso: Boolean;
    FNumeroNota: string;
    FSerieNota: string;
    FData: TDateTime;
    FidNota: string;
    FLink: String;
    FProtocolo: String;
    FNumeroRps: string;
    FSerieRps: string;

    FAlertas: TNFSeEventoCollection;
    FErros: TNFSeEventoCollection;

    FEnvelopeEnvio: String;
    FEnvelopeRetorno: String;
    FArquivoEnvio: String;
    FArquivoRetorno: String;

    function GetXmlEnvio: String;
    procedure SetXmlEnvio(const Value: String);
    function GetXmlRetorno: String;
    procedure SetXmlRetorno(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    property Situacao: String read FSituacao write FSituacao;
    property DescSituacao: String read FDescSituacao write FDescSituacao;
    property Lote: string read FLote write FLote;
    property Sucesso: Boolean read FSucesso write FSucesso;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property SerieNota: string read FSerieNota write FSerieNota;
    property Data: TDateTime read FData write FData;
    property idNota: string read FidNota write FidNota;
    property Link: String read FLink write FLink;
    property Protocolo: String read FProtocolo write FProtocolo;
    property NumeroRps: string read FNumeroRps write FNumeroRps;
    property SerieRps: string read FSerieRps write FSerieRps;

    property Alertas: TNFSeEventoCollection read FAlertas;
    property Erros: TNFSeEventoCollection read FErros;

    property XmlEnvio: String read GetXmlEnvio write SetXmlEnvio;
    property XmlRetorno: String read GetXmlRetorno write SetXmlRetorno;

    property EnvelopeEnvio: String read FEnvelopeEnvio write FEnvelopeEnvio;
    property EnvelopeRetorno: String read FEnvelopeRetorno write FEnvelopeRetorno;
    property ArquivoEnvio: String read FArquivoEnvio write FArquivoEnvio;
    property ArquivoRetorno: String read FArquivoRetorno write FArquivoRetorno;
  end;

  TNFSeEmiteResponse = class(TNFSeWebserviceResponse)
  private
    FModoEnvio: TmodoEnvio;
    FMaxRps: Integer;
    FCodVerificacao: string;
    FNomeArq: string;
  public
    constructor Create;
    destructor Destroy; override;

    property MaxRps: Integer read FMaxRps write FMaxRps;
    property ModoEnvio: TmodoEnvio read FModoEnvio write FModoEnvio;
    property CodVerificacao: string read FCodVerificacao write FCodVerificacao;
    property NomeArq: string read FNomeArq write FNomeArq;
  end;

  TNFSeConsultaSituacaoResponse = class(TNFSeWebserviceResponse)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
  end;

  TNFSeConsultaLoteRpsResponse = class(TNFSeWebserviceResponse)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
  end;

  TNFSeConsultaNFSeporRpsResponse = class(TNFSeWebserviceResponse)
  private
    FNumRPS: string;
    FSerie: string;
    FTipo: string;
    FCodVerificacao: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property NumRPS: string read FNumRPS write FNumRPS;
    property Serie: string read FSerie write FSerie;
    property Tipo: string read FTipo write FTipo;
    property CodVerificacao: string read FCodVerificacao write FCodVerificacao;
  end;

  TNFSeConsultaNFSeResponse = class(TNFSeWebserviceResponse)
  private
    FInfConsultaNFSe: TInfConsultaNFSe;
    FMetodo: TMetodo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property Metodo: TMetodo read FMetodo write FMetodo;
    property InfConsultaNFSe: TInfConsultaNFSe read FInfConsultaNFSe write FInfConsultaNFSe;
  end;

  TNFSeCancelaNFSeResponse = class(TNFSeWebserviceResponse)
  private
    FInfCancelamento: TInfCancelamento;
    FRetCancelamento: TRetCancelamento;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property InfCancelamento: TInfCancelamento read FInfCancelamento write FInfCancelamento;
    property RetCancelamento: TRetCancelamento read FRetCancelamento;
  end;

  TNFSeSubstituiNFSeResponse = class(TNFSeCancelaNFSeResponse)
  private
    FNumRPS: string;
    FSerie: string;
    FTipo: string;
    FCodVerificacao: string;
    FPedCanc: string;
    FNumNotaSubstituida: string;
    FNumNotaSubstituidora: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property NumRPS: string read FNumRPS write FNumRPS;
    property Serie: string read FSerie write FSerie;
    property Tipo: string read FTipo write FTipo;
    property CodVerificacao: string read FCodVerificacao write FCodVerificacao;
    property PedCanc: string read FPedCanc write FPedCanc;
    property NumNotaSubstituida: string read FNumNotaSubstituida write FNumNotaSubstituida;
    property NumNotaSubstituidora: string read FNumNotaSubstituidora write FNumNotaSubstituidora;
  end;

  TNFSeAbreSessaoResponse = class(TNFSeWebserviceResponse)
  private
    FHashIdent: String;
  public
    property HashIdent: String read FHashIdent write FHashIdent;
  end;

  TNFSeFechaSessaoResponse = class(TNFSeWebserviceResponse)
  private
    FHashIdent: String;
  public
    property HashIdent: String read FHashIdent write FHashIdent;
  end;

implementation

uses
  SysUtils;

{ TNFSeEventoCollection }

function TNFSeEventoCollection.GetItem(Index: Integer): TNFSeEventoCollectionItem;
begin
  Result := TNFSeEventoCollectionItem(inherited Items[Index]);
end;

procedure TNFSeEventoCollection.SetItem(Index: Integer; Value: TNFSeEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TNFSeEventoCollection.New: TNFSeEventoCollectionItem;
begin
  Result := TNFSeEventoCollectionItem.Create;
  Self.Add(Result);
end;

function TNFSeEventoCollection.Add(ANota: TNFSeEventoCollectionItem): Integer;
begin
  Result := inherited Add(ANota);
end;

Procedure TNFSeEventoCollection.Insert(Index: Integer; ANota: TNFSeEventoCollectionItem);
begin
  inherited Insert(Index, ANota);
end;

{ TNotasCanceladasCollection }

function TNotasCanceladasCollection.GetItem(Index: Integer): TNotasCanceladasCollectionItem;
begin
  Result := TNotasCanceladasCollectionItem(inherited Items[Index]);
end;

procedure TNotasCanceladasCollection.SetItem(Index: Integer; Value: TNotasCanceladasCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TNotasCanceladasCollection.New: TNotasCanceladasCollectionItem;
begin
  Result := TNotasCanceladasCollectionItem.Create;
  Self.Add(Result);
end;

function TNotasCanceladasCollection.Add(ANota: TNotasCanceladasCollectionItem): Integer;
begin
  Result := inherited Add(ANota);
end;

Procedure TNotasCanceladasCollection.Insert(Index: Integer; ANota: TNotasCanceladasCollectionItem);
begin
  inherited Insert(Index, ANota);
end;

{ TRetCancelamento }

constructor TRetCancelamento.Create;
begin
  inherited Create;

  FPedido := TPedidoCancelamento.Create;
  FNotasCanceladas := TNotasCanceladasCollection.Create;
end;

destructor TRetCancelamento.Destroy;
begin
  FPedido.Free;
  FNotasCanceladas.Free;

  inherited Destroy;
end;

{ TNFSeWebserviceResponse }

procedure TNFSeWebserviceResponse.Clear;
var
  i: Integer;
begin
  Situacao := '';
  DescSituacao := '';
  Lote := '';
  Sucesso := False;
  NumeroNota := '';
  SerieNota := '';
  Data := 0;
  idNota := '';
  Link := '';
  Protocolo := '';
  NumeroRps := '';
  SerieRps := '';

  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;

  XmlEnvio := '';
  XmlRetorno := '';
  EnvelopeEnvio := '';
  EnvelopeRetorno := '';
  ArquivoEnvio := '';
  ArquivoRetorno := '';
end;

constructor TNFSeWebserviceResponse.Create;
begin
  inherited Create;

  FSucesso := False;
  FAlertas := TNFSeEventoCollection.Create;
  FErros := TNFSeEventoCollection.Create;
end;

destructor TNFSeWebserviceResponse.Destroy;
begin
  FAlertas.Free;
  FErros.Free;

  inherited;
end;

function TNFSeWebserviceResponse.GetXmlEnvio: String;
begin
  Result := ArquivoEnvio;
end;

function TNFSeWebserviceResponse.GetXmlRetorno: String;
begin
  Result := ArquivoRetorno;
end;

procedure TNFSeWebserviceResponse.SetXmlEnvio(const Value: String);
begin
  ArquivoEnvio := Value;
end;

procedure TNFSeWebserviceResponse.SetXmlRetorno(const Value: String);
begin
  ArquivoRetorno := Value;
end;

{ TNFSeConsultaNFSeResponse }

constructor TNFSeConsultaNFSeResponse.Create;
begin
  inherited Create;

  Clear;
end;

procedure TNFSeConsultaNFSeResponse.Clear;
var
  i: Integer;
begin
  if Assigned(FInfConsultaNFSe) then
    FInfConsultaNFSe.Free;

  FMetodo := tmConsultarNFSe;

  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;

  FInfConsultaNFSe := TInfConsultaNFSe.Create;
end;

destructor TNFSeConsultaNFSeResponse.Destroy;
begin
  FInfConsultaNFSe.Free;

  inherited Destroy;
end;

{ TNFSeCancelaNFSeResponse }

constructor TNFSeCancelaNFSeResponse.Create;
begin
  inherited Create;

  Clear;
end;

procedure TNFSeCancelaNFSeResponse.Clear;
var
  i: Integer;
begin
  if Assigned(FInfCancelamento) then
    FInfCancelamento.Free;

  if Assigned(FRetCancelamento) then
   FRetCancelamento.Free;

  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;

  FInfCancelamento := TInfCancelamento.Create;
  FRetCancelamento := TRetCancelamento.Create;
end;

destructor TNFSeCancelaNFSeResponse.Destroy;
begin
  if Assigned(FInfCancelamento) then
    FInfCancelamento.Free;

  if Assigned(FRetCancelamento) then
    FRetCancelamento.Free;

  inherited Destroy;
end;

{ TNFSeSubstituiNFSeResponse }

procedure TNFSeSubstituiNFSeResponse.Clear;
var
  i: Integer;
begin
  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;
end;

constructor TNFSeSubstituiNFSeResponse.Create;
begin
  inherited Create;

  FInfCancelamento := TInfCancelamento.Create;
  FRetCancelamento := TRetCancelamento.Create;
end;

destructor TNFSeSubstituiNFSeResponse.Destroy;
begin
  if Assigned(FInfCancelamento) then
    FreeAndNil(FInfCancelamento);

  if Assigned(FRetCancelamento) then
    FreeAndNil(FRetCancelamento);

  inherited Destroy;
end;

{ TNFSeEmiteResponse }

constructor TNFSeEmiteResponse.Create;
begin
  inherited Create;

end;

destructor TNFSeEmiteResponse.Destroy;
begin

  inherited Destroy;
end;

{ TNFSeConsultaSituacaoResponse }

procedure TNFSeConsultaSituacaoResponse.Clear;
var
  i: Integer;
begin
  Lote := '';
  Situacao := '';
  Protocolo := '';

  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;
end;

constructor TNFSeConsultaSituacaoResponse.Create;
begin
  inherited Create;

end;

destructor TNFSeConsultaSituacaoResponse.Destroy;
begin

  inherited Destroy;
end;

{ TNFSeConsultaLoteRpsResponse }

procedure TNFSeConsultaLoteRpsResponse.Clear;
var
  i: Integer;
begin
  Lote := '';
  Protocolo := '';
  Situacao := '';

  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;
end;

constructor TNFSeConsultaLoteRpsResponse.Create;
begin
  inherited Create;

end;

destructor TNFSeConsultaLoteRpsResponse.Destroy;
begin

  inherited Destroy;
end;

{ TNFSeConsultaNFSeporRpsResponse }

procedure TNFSeConsultaNFSeporRpsResponse.Clear;
var
  i: Integer;
begin
  if Assigned(FErros) then
  begin
    for i := FErros.Count - 1 downto 0 do
      FErros.Delete(i);
  end;

  if Assigned(FAlertas) then
  begin
    for i := FAlertas.Count - 1 downto 0 do
      FAlertas.Delete(i);
  end;
end;

constructor TNFSeConsultaNFSeporRpsResponse.Create;
begin
  inherited Create;

end;

destructor TNFSeConsultaNFSeporRpsResponse.Destroy;
begin

  inherited Destroy;
end;

{ TNFSeParamsResponse }

procedure TNFSeParamsResponse.Clear;
begin
  Xml := '';
  TagEnvio := '';
  Prefixo := '';
  Prefixo2 := '';
  NameSpace := '';
  NameSpace2 := '';
  IdAttr := '';
  Versao := '';
  Serie := '';
  Motivo := '';
  CodVerif := '';
end;

end.
