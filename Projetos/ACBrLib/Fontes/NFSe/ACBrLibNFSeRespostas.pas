{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
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
  ACBrLibResposta, ACBrNFSeXNotasFiscais, ACBrNFSeX, ACBrNFSeXWebServices;

type

  { TLibEnvioRespostaItem }
  TLibEnvioRespostaItem = class(TACBrLibRespostaBase)
  private
    FArquivo: String;
    FNFSe: String;
    FCodigoVerificacao: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Itens: Integer; const Notas: TNotasFiscais);

  published
    property Arquivo: String read FArquivo write FArquivo;
    property NFSe: String read FNFSe write FNFSe;
    property CodigoVerificacao: String read FCodigoVerificacao write FCodigoVerificacao;

  end;

  { TLibNFSeServiceResposta }
  TLibNFSeServiceResposta = class abstract(TACBrLibResposta<TACBrNFSeX>)
  private

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    procedure Processar(const ACBrNFSeX: TACBrNFSeX); virtual; abstract; reintroduce;

  end;

  { TEnvioResposta }
  TEnvioResposta = class(TLibNFSeServiceResposta)
  private
    FXML: String;
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;
    FSucesso: String;
    FSituacao: String;
    FNumeroNota: Integer;
    FLink: String;
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrNFSeX: TACBrNFSeX); override;

  published
    property XML: String read FXML write FXML;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property DataRecebimento: TDateTime read FDataRecebimento write FDataRecebimento;
    property Protocolo: String read FProtocolo write FProtocolo;
    property Sucesso: String read FSucesso write FSucesso;
    property Situacao: String read FSituacao write FSituacao;
    property NumeroNota: Integer read FNumeroNota write FNumeroNota;
    property Link: String read FLink write FLink;
    property Items: TObjectList read FItems;

  end;

  { TConsultaSituacaoLoteResposta }
  TConsultaSituacaoLoteResposta = class(TACBrLibRespostaBase)
  private
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ConsultarSituacaoLoteRPS: TNFSeConsultarSituacaoLoteRPS);

  published
    property XML: String read FXML write FXML;

  end;

  { TConsultaLoteResposta }
  TConsultaLoteResposta = class(TACBrLibRespostaBase)
  private
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ConsultarLoteRPS: TNFSeConsultarLoteRPS);

  published
    property XML: String read FXML write FXML;

  end;

  { TConsultaNFSeporRPSResposta }
  TConsultaNFSeporRPSResposta = class(TACBrLibRespostaBase)
  private
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ConsultarNFSeRPS: TNFSeConsultarNFSeRps);

  published
    property XML: String read FXML write FXML;

  end;

  { TConsultaNFSeResposta }
  TConsultaNFSeResposta = class(TACBrLibRespostaBase)
  private
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ConsultarNFSe: TNFSeConsultarNFSe);

  published
    property XML: String read FXML write FXML;

  end;

  { TCancelarNFSeResposta }
  TCancelarNFSeResposta = class(TACBrLibRespostaBase)
  private
    FNumeroLote: String;
    FSituacao: String;
    FDataHora: TDateTime;
    FMsgCanc: String;
    FSucesso: String;
    FLink: String;
    FNumeroNota: Integer;
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const CancelarNFSe: TNFSeCancelarNFSe);

  published
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Situacao: String read FSituacao write FSituacao;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property MsgCanc: String read FMsgCanc write FMsgCanc;
    property Sucesso: String read FSucesso write FSucesso;
    property Link: String read FLink write FLink;
    property NumeroNota: Integer read FNumeroNota write FNumeroNota;
    property XML: String read FXML write FXML;

  end;

   { TLinkResposta }
  TLinkResposta = class(TACBrLibRespostaBase)
  private
    FLink: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao; const ALink: String); reintroduce;
    destructor Destroy; override;

    procedure Processar();

  published
    property Link: String read FLink write FLink;

  end;

  { TSubstituirNFSeResposta }
  TSubstituirNFSeResposta = class(TACBrLibRespostaBase)
  private
    FXML: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const SubstituirNFSe: TNFSeSubstituirNFSe);

  published
    property XML: String read FXML write FXML;

  end;



implementation

uses
  pcnAuxiliar, pcnConversao,
  ACBrUtil, ACBrLibNFSeConsts;

{ TSubstituirNFSeResposta }

constructor TSubstituirNFSeResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespSubstituir, ATipo, AFormato);
end;

destructor TSubstituirNFSeResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TSubstituirNFSeResposta.Processar(
  const SubstituirNFSe: TNFSeSubstituirNFSe);
begin
  FXML:= SubstituirNFSe.RetSubsNFSe.InfNFSe.XML;
end;

{ TLinkResposta }

constructor TLinkResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao; const ALink: String);
begin
  inherited Create(CSessaoRespLink, ATipo, AFormato);
  FLink:= ALink;
end;

destructor TLinkResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLinkResposta.Processar();
begin
  inherited;
end;


{ TCancelarNFSeResposta }

constructor TCancelarNFSeResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelar, ATipo, AFormato);
end;

destructor TCancelarNFSeResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TCancelarNFSeResposta.Processar(const CancelarNFSe: TNFSeCancelarNFSe);
begin
  FNumeroLote:= CancelarNFSe.RetCancNFSe.InfCanc.NumeroLote;
  FSituacao:= CancelarNFSe.RetCancNFSe.InfCanc.Situacao;
  FDataHora:= CancelarNFSe.RetCancNFSe.InfCanc.DataHora;
  FMsgCanc:= CancelarNFSe.RetCancNFSe.InfCanc.MsgCanc;
  FSucesso:= CancelarNFSe.RetCancNFSe.InfCanc.Sucesso;
  FLink:= CancelarNFSe.RetCancNFSe.InfCanc.Link;
  FNumeroNota:= CancelarNFSe.RetCancNFSe.InfCanc.NumeroNota;
  FXML:= CancelarNFSe.RetCancNFSe.InfCanc.XML;

end;

{ TConsultaNFSeResposta }

constructor TConsultaNFSeResposta.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

destructor TConsultaNFSeResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaNFSeResposta.Processar(
  const ConsultarNFSe: TNFSeConsultarNFSe);
begin
  FXML:= ConsultarNFSe.RetConsNFSe.InfNFSe.XML;
end;

{ TConsultaNFSeporRPSResposta }

constructor TConsultaNFSeporRPSResposta.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

destructor TConsultaNFSeporRPSResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaNFSeporRPSResposta.Processar(const ConsultarNFSeRPS: TNFSeConsultarNFSeRps);
begin
  FXML:= ConsultarNFSeRPS.RetConsNFSe.InfNFSe.XML;
end;

{ TConsultaLoteResposta }

constructor TConsultaLoteResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

destructor TConsultaLoteResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaLoteResposta.Processar(
  const ConsultarLoteRPS: TNFSeConsultarLoteRPS);
begin
  FXML:= ConsultarLoteRPS.RetConsLote.InfLote.XML;
end;

{ TConsultaSituacaoLoteResposta }

constructor TConsultaSituacaoLoteResposta.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

destructor TConsultaSituacaoLoteResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaSituacaoLoteResposta.Processar(const ConsultarSituacaoLoteRPS: TNFSeConsultarSituacaoLoteRPS);
begin
  FXML:= ConsultarSituacaoLoteRPS.RetSitLote.InfSit.XML;

end;

{ TLibEnvioRespostaItem }

constructor TLibEnvioRespostaItem.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TLibEnvioRespostaItem.Processar(const Itens: Integer; const Notas: TNotasFiscais);
begin
  FArquivo:= Notas.Items[Itens].NomeArq;
  FNFSe:= Notas.Items[Itens].NFSe.Numero;
  FCodigoVerificacao:= Notas.Items[Itens].NFSe.CodigoVerificacao;
end;

{ TLibNFSeServiceResposta }
constructor TLibNFSeServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioResposta }
constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
  FItems := TObjectList.Create(True);
end;

destructor TEnvioResposta.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TEnvioResposta.Processar(const ACBrNFSeX: TACBrNFSeX);
var
  RespostaItem: TLibEnvioRespostaItem;
  item: Integer;
begin
  with ACBrNFSeX.WebServices.Emitir.RetEnviarRps do
  begin
    XML:= InfRet.XML;
    NumeroLote:= InfRet.NumeroLote;
    DataRecebimento:= InfRet.DataRecebimento;
    Protocolo:= InfRet.Protocolo;
    Sucesso:= InfRet.Sucesso;
    Situacao:= InfRet.Situacao;
    NumeroNota:= InfRet.NumeroNota;
    Link:= InfRet.Link;

  end;
  FItems.Clear;

  for item := 0 to ACBrNFSeX.NotasFiscais.Count - 1 do
  begin
    RespostaItem := TLibEnvioRespostaItem.Create('NFSe' + ACBrNFSeX.NotasFiscais.Items[item].NFSe.Numero, Tipo, Formato);
    RespostaItem.Processar(Item, ACBrNFSeX.NotasFiscais);
    FItems.Add(RespostaItem);
  end;


end;

end.

