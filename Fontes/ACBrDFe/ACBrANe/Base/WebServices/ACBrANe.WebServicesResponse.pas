{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrANe.WebServicesResponse;

interface

uses
  classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrANe.Classes, ACBrANe.Conversao, ACBrANe.WebServicesBase;

type
  TANeEventoCollectionItem = class
  private
    FCodigo: string;
    FDescricao: string;
    FCorrecao: string;
  public
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

  TANeEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TANeEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TANeEventoCollectionItem);
  public
    function New: TANeEventoCollectionItem;
    function Add(ANota: TANeEventoCollectionItem): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TANeEventoCollectionItem); reintroduce;

    property Items[Index: Integer]: TANeEventoCollectionItem read GetItem write SetItem; default;
  end;

  TANeResumoCollectionItem = class
  private
    FNumeroNota: string;
    FCodigoVerificacao: string;
    FNumeroRps: string;
    FSerieRps: string;
    FSituacao: string;
    FDescSituacao: string;
    FLink: string;
    FProtocolo: string;
    FSerieNota: string;
    FData: TDateTime;
    FNSU: Integer;
    FChaveDFe: string;
    FTipoDoc: string;
    FidNota: string;
    FNomeArq: string;
  public
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property CodigoVerificacao: string read FCodigoVerificacao write FCodigoVerificacao;
    property NumeroRps: string read FNumeroRps write FNumeroRps;
    property SerieRps: string read FSerieRps write FSerieRps;
    property Situacao: string read FSituacao write FSituacao;
    property DescSituacao: string read FDescSituacao write FDescSituacao;
    property Link: string read FLink write FLink;
    property Protocolo: string read FProtocolo write FProtocolo;
    property SerieNota: string read FSerieNota write FSerieNota;
    property Data: TDateTime read FData write FData;
    property NSU: Integer read FNSU write FNSU;
    property ChaveDFe: string read FChaveDFe write FChaveDFe;
    property TipoDoc: string read FTipoDoc write FTipoDoc;
    property idNota: string read FidNota write FidNota;
    property NomeArq: string read FNomeArq write FNomeArq;
  end;

  TANeResumoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TANeResumoCollectionItem;
    procedure SetItem(Index: Integer; Value: TANeResumoCollectionItem);
  public
    function New: TANeResumoCollectionItem;
    function Add(ANota: TANeResumoCollectionItem): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TANeResumoCollectionItem); reintroduce;

    property Items[Index: Integer]: TANeResumoCollectionItem read GetItem write SetItem; default;
  end;

  TANeParamsResponse = class
  private
    FXml: string;
    FTagEnvio: string;
    FPrefixo: string;
    FPrefixo2: string;
    FNameSpace: string;
    FNameSpace2: string;
    FIdAttr: string;
    FVersao: string;
    FSerie: string;
    FMotivo: string;
    FCodigoVerificacao: string;
  public
    procedure Clear;

    property Xml: string read FXml write FXml;
    property TagEnvio: string read FTagEnvio write FTagEnvio;
    property Prefixo: string read FPrefixo write FPrefixo;
    property Prefixo2: string read FPrefixo2 write FPrefixo2;
    property NameSpace: string read FNameSpace write FNameSpace;
    property NameSpace2: string read FNameSpace2 write FNameSpace2;
    property IdAttr: string read FIdAttr write FIdAttr;
    property Versao: string read FVersao write FVersao;
    property Serie: string read FSerie write FSerie;
    property Motivo: string read FMotivo write FMotivo;
    property CodigoVerificacao: string read FCodigoVerificacao write FCodigoVerificacao;
  end;

  TANeWebserviceResponse = class
  private
    FSucesso: Boolean;

    FNumero: string;
    FSerie: string;
    FFilial: string;
    FCNPJCliente: string;
    FtpDoc: string;
    FDataHora: TDateTime;
    FProtocolo: string;
    FNumeroAverbacao: string;
    FStatus: string;

    FAlertas: TANeEventoCollection;
    FErros: TANeEventoCollection;
    FResumos: TANeResumoCollection;

    FEnvelopeEnvio: string;
    FEnvelopeRetorno: string;
    FArquivoEnvio: string;
    FArquivoRetorno: string;

    function GetXmlEnvio: string;
    procedure SetXmlEnvio(const Value: string);
    function GetXmlRetorno: string;
    procedure SetXmlRetorno(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    property Sucesso: Boolean read FSucesso write FSucesso;

    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Filial: string read FFilial write FFilial;
    property CNPJCliente: string read FCNPJCliente write FCNPJCliente;
    property tpDoc: string read FtpDoc write FtpDoc;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property Protocolo: string read FProtocolo write FProtocolo;
    property NumeroAverbacao: string read FNumeroAverbacao write FNumeroAverbacao;
    property Status: string read FStatus write FStatus;

    property Alertas: TANeEventoCollection read FAlertas;
    property Erros: TANeEventoCollection read FErros;
    property Resumos: TANeResumoCollection read FResumos;

    property XmlEnvio: string read GetXmlEnvio write SetXmlEnvio;
    property XmlRetorno: string read GetXmlRetorno write SetXmlRetorno;

    property EnvelopeEnvio: string read FEnvelopeEnvio write FEnvelopeEnvio;
    property EnvelopeRetorno: string read FEnvelopeRetorno write FEnvelopeRetorno;
    property ArquivoEnvio: string read FArquivoEnvio write FArquivoEnvio;
    property ArquivoRetorno: string read FArquivoRetorno write FArquivoRetorno;
  end;

  TANeEnviarResponse = class(TANeWebserviceResponse)
  private
    FMaxRps: Integer;
    FMinRps: Integer;
    FCodigoVerificacao: string;
    FNomeArq: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property MaxRps: Integer read FMaxRps write FMaxRps;
    property MinRps: Integer read FMinRps write FMinRps;
    property CodigoVerificacao: string read FCodigoVerificacao write FCodigoVerificacao;
    property NomeArq: string read FNomeArq write FNomeArq;
  end;

  TANeConsultarResponse = class(TANeWebserviceResponse)
  private
    FChave: string;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property Chave: string read FChave write FChave;
  end;

implementation

uses
  SysUtils;

{ TANeEventoCollection }

function TANeEventoCollection.GetItem(Index: Integer): TANeEventoCollectionItem;
begin
  Result := TANeEventoCollectionItem(inherited Items[Index]);
end;

procedure TANeEventoCollection.SetItem(Index: Integer; Value: TANeEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TANeEventoCollection.New: TANeEventoCollectionItem;
begin
  Result := TANeEventoCollectionItem.Create;
  Self.Add(Result);
end;

function TANeEventoCollection.Add(ANota: TANeEventoCollectionItem): Integer;
begin
  Result := inherited Add(ANota);
end;

Procedure TANeEventoCollection.Insert(Index: Integer; ANota: TANeEventoCollectionItem);
begin
  inherited Insert(Index, ANota);
end;

{ TANeWebserviceResponse }

procedure TANeWebserviceResponse.Clear;
var
  i: Integer;
begin
  Numero := '';
  Serie := '';
  Filial := '';
  CNPJCliente := '';
  tpDoc := '';
  DataHora := 0;
  Protocolo := '';
  NumeroAverbacao := '';
  Status := '';

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

  if Assigned(FResumos) then
  begin
    for i := FResumos.Count - 1 downto 0 do
      FResumos.Delete(i);
  end;

  XmlEnvio := '';
  XmlRetorno := '';
  EnvelopeEnvio := '';
  EnvelopeRetorno := '';
  ArquivoEnvio := '';
  ArquivoRetorno := '';
end;

constructor TANeWebserviceResponse.Create;
begin
  inherited Create;

  FSucesso := False;
  FAlertas := TANeEventoCollection.Create;
  FErros := TANeEventoCollection.Create;
  FResumos := TANeResumoCollection.Create;
end;

destructor TANeWebserviceResponse.Destroy;
begin
  FAlertas.Free;
  FErros.Free;
  FResumos.Free;

  inherited;
end;

function TANeWebserviceResponse.GetXmlEnvio: string;
begin
  Result := ArquivoEnvio;
end;

function TANeWebserviceResponse.GetXmlRetorno: string;
begin
  Result := ArquivoRetorno;
end;

procedure TANeWebserviceResponse.SetXmlEnvio(const Value: string);
begin
  ArquivoEnvio := Value;
end;

procedure TANeWebserviceResponse.SetXmlRetorno(const Value: string);
begin
  ArquivoRetorno := Value;
end;

{ TANeEnviarResponse }

procedure TANeEnviarResponse.Clear;
begin
  inherited Clear;

  MaxRps := 0;
  MinRps := 0;
  CodigoVerificacao := '';
  NomeArq := '';
end;

constructor TANeEnviarResponse.Create;
begin
  inherited Create;

end;

destructor TANeEnviarResponse.Destroy;
begin

  inherited Destroy;
end;

{ TANeConsultarResponse }

procedure TANeConsultarResponse.Clear;
begin
  inherited Clear;

  Chave := '';
end;

constructor TANeConsultarResponse.Create;
begin
  inherited Create;

end;

destructor TANeConsultarResponse.Destroy;
begin

  inherited Destroy;
end;

{ TANeParamsResponse }

procedure TANeParamsResponse.Clear;
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
  CodigoVerificacao := '';
end;

{ TANeResumoCollection }

function TANeResumoCollection.Add(ANota: TANeResumoCollectionItem): Integer;
begin
  Result := inherited Add(ANota);
end;

function TANeResumoCollection.GetItem(
  Index: Integer): TANeResumoCollectionItem;
begin
  Result := TANeResumoCollectionItem(inherited Items[Index]);
end;

procedure TANeResumoCollection.Insert(Index: Integer;
  ANota: TANeResumoCollectionItem);
begin
  inherited Insert(Index, ANota);
end;

function TANeResumoCollection.New: TANeResumoCollectionItem;
begin
  Result := TANeResumoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TANeResumoCollection.SetItem(Index: Integer;
  Value: TANeResumoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
