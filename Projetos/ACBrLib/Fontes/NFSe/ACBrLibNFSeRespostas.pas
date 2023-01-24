{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                            }
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

{$I ACBr.inc}

unit ACBrLibNFSeRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  ACBrLibResposta, ACBrNFSeXNotasFiscais, ACBrNFSeX, ACBrNFSeXWebservicesResponse;

type

  { TNFSeEventoItem }
  TNFSeEventoItem = class(TACBrLibRespostaBase)
  private
    FCodigo: String;
    FDescricao: String;
    FCorrecao: String;

  public
    procedure Processar(const Evento: TNFSeEventoCollectionItem);

  published
    property Codigo: String read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Correcao: String read FCorrecao write FCorrecao;

  end;

  { TLibNFSeServiceResposta }
  TLibNFSeServiceResposta = class abstract(TACBrLibRespostaBase)
  private
    FXmlEnvio: string;
    FXmlRetorno: string;
    FErros: TObjectList;
    FAlertas: TObjectList;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    destructor Destroy;

    procedure Processar(const Response: TNFSeWebserviceResponse); virtual;

  published
    property XmlEnvio: String read FXmlEnvio write FXmlEnvio;
    property XmlRetorno: String read FXmlRetorno write FXmlRetorno;

  end;

  { TEmiteResposta }
  TEmiteResposta = class(TLibNFSeServiceResposta)
  private
    FLote: string;
    FData: TDateTime;
    FProtocolo: string;
    FModoEnvio: string;
    FMaxRps: Integer;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy;

    procedure Processar(const Response: TNFSeEmiteResponse); reintroduce;

  published
    property Lote: string read FLote write FLote;
    property Data: TDateTime read FData write FData;
    property Protocolo: String read FProtocolo write FProtocolo;
    property MaxRps: Integer read FMaxRps write FMaxRps;
    property ModoEnvio: string read FModoEnvio write FModoEnvio;

  end;

  { TConsultaSituacaoResposta }
  TConsultaSituacaoResposta = class(TLibNFSeServiceResposta)
  Private
    FLote: string;
    FSituacao: string;
    FProtocolo: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy;

    procedure Processar(const Response: TNFSeConsultaSituacaoResponse); reintroduce;

  published
    property Lote: string read FLote write FLote;
    property Protocolo: string read FProtocolo write FProtocolo;
    property Situacao: string read FSituacao write FSituacao;

  end;

implementation

uses
  pcnAuxiliar, pcnConversao,
  ACBrNFSeXConversao,
  ACBrUtil, ACBrLibNFSeConsts;

{ TNFSeEventoItem }
procedure TNFSeEventoItem.Processar(const Evento: TNFSeEventoCollectionItem);
begin
  Codigo := Evento.Codigo;
  Descricao := Evento.Descricao;
  Correcao := Evento.Correcao;
end;

{ TLibNFSeServiceResposta }
constructor TLibNFSeServiceResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);

  FErros := TObjectList.Create(True);
  FAlertas := TObjectList.Create(True);
end;

destructor TLibNFSeServiceResposta.Destroy;
begin

  FErros.Destroy;
  FAlertas.Destroy;
  inherited Destroy;
end;

procedure TLibNFSeServiceResposta.Processar(const Response: TNFSeWebserviceResponse);
var
  i: Integer;
  Item: TNFSeEventoItem;
begin
  XmlEnvio := Response.XmlEnvio;
  XmlRetorno := Response.XmlRetorno;

  if Response.Erros.Count > 0 then
  begin
    for i := 0 to Response.Erros.Count -1 do
    begin
      Item := TNFSeEventoItem.Create(CSessaoRespErro + IntToStr(i + 1), Tipo, Formato);
      Item.Processar(Response.Erros.Items[i]);
      FErros.Add(Item);
    end;
  end;

  if Response.Alertas.Count > 0 then
  begin
    for i := 0 to Response.Alertas.Count -1 do
    begin
      Item := TNFSeEventoItem.Create(CSessaoRespAlerta + IntToStr(i + 1), Tipo, Formato);
      Item.Processar(Response.Alertas.Items[i]);
      FAlertas.Add(Item);
    end;
  end;
end;

{ TEmiteResposta }
constructor TEmiteResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

destructor TEmiteResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TEmiteResposta.Processar(const Response: TNFSeEmiteResponse);
begin
  inherited Processar(Response);

  Lote := Response.Lote;
  Data := Response.Data;
  Protocolo := Response.Protocolo;
  MaxRps := Response.MaxRps;
  ModoEnvio := ModoEnvioToStr(Response.ModoEnvio);
end;

{ TConsultaSituacaoResposta }
constructor TConsultaSituacaoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespSituacao, ATipo, AFormato);
end;

destructor TConsultaSituacaoResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaSituacaoResposta.Processar(const Response: TNFSeConsultaSituacaoResponse);
begin
  inherited Processar(Response);

  Lote := Response.Lote;
  Protocolo := Response.Protocolo;
  Situacao := Response.Situacao;
end;

end.

