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
|* 29/02/2015: Guilherme Costa
|*  - não estava sendo gerada a tag "tpProc"
******************************************************************************}

{$I ACBr.inc}

unit pcesRetConsultaLote;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pcesCommon, pcesRetornoClass, pcesConversaoeSocial;

type
  TRetEventosCollection = class;
  TRetEventosCollectionItem = class;
  TRetConsultaLote = class;

  TRetEventosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetEventosCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventosCollectionItem);
  public
    constructor create(AOwner: TRetConsultaLote);

    function Add: TRetEventosCollectionItem;
    property Items[Index: Integer]: TRetEventosCollectionItem read GetItem write SetItem;
  end;

  TRetEventosCollectionItem = class(TCollectionItem)
  private
    FIDEvento: string;
    FRecepcao: TRecepcao;
    FProcessamento: TProcessamento;
    FRecibo: TRecibo;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property IDEvento: string read FIDEvento write FIDEvento;
    property Recepcao: TRecepcao read FRecepcao write FRecepcao;
    property Processamento: TProcessamento read FProcessamento write FProcessamento;
    property Recibo: TRecibo read FRecibo write FRecibo;
  end;

  TRetConsultaLote = class(TPersistent)
  private
    FLeitor: TLeitor;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTransmissor: TIdeTransmissor;
    FStatus: TStatus;
    FDadosRecLote: TDadosRecepcaoLote;
    FDadosProcLote: TDadosProcLote;
    FRetEventos: TRetEventosCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;

    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador;
    property IdeTransmissor: TIdeTransmissor read FIdeTransmissor;
    property Status: TStatus read FStatus;
    property DadosRecLote: TDadosRecepcaoLote read FDadosRecLote;
    property DadosProcLote: TDadosProcLote read FDadosProcLote;
    property RetEventos: TRetEventosCollection read FRetEventos;
  end;

implementation

{ TRetEventosCollection }

function TRetEventosCollection.Add: TRetEventosCollectionItem;
begin
  Result := TRetEventosCollectionItem(inherited Add());
end;

constructor TRetEventosCollection.create(AOwner: TRetConsultaLote);
begin
  inherited create(TRetEventosCollectionItem);
end;

function TRetEventosCollection.GetItem(
  Index: Integer): TRetEventosCollectionItem;
begin
  Result := TRetEventosCollectionItem(Inherited GetItem(Index));
end;

procedure TRetEventosCollection.SetItem(Index: Integer;
  Value: TRetEventosCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TRetEventosCollectionItem }

constructor TRetEventosCollectionItem.Create;
begin
  FRecepcao := TRecepcao.Create;
  FProcessamento := TProcessamento.Create;
  FRecibo := TRecibo.Create;
end;

destructor TRetEventosCollectionItem.Destroy;
begin
  FRecepcao.Free;
  FProcessamento.Free;
  FRecibo.Free;

  inherited;
end;

{ TRetConsultaLote }

constructor TRetConsultaLote.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FStatus         := TStatus.Create;
  FDadosRecLote   := TDadosRecepcaoLote.Create;
  FDadosProcLote  := TDadosProcLote.Create;
  FRetEventos     := TRetEventosCollection.create(Self);
end;

destructor TRetConsultaLote.Destroy;
begin
  FLeitor.Free;
  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FStatus.Free;
  FDadosRecLote.Free;
  FDadosProcLote.Free;
  FRetEventos.Free;

  inherited;
end;

function TRetConsultaLote.LerXml: boolean;
var
  ok: boolean;
  i, j: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if leitor.rExtrai(1, 'retornoProcessamentoLoteEventos') <> '' then
    begin
      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(Ok, FLeitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := FLeitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideTransmissor') <> '' then
      begin
        IdeTransmissor.TpInsc := eSStrToTpInscricao(Ok, FLeitor.rCampo(tcStr, 'tpInsc'));
        IdeTransmissor.NrInsc := FLeitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'status') <> '' then
      begin
        Status.cdResposta   := Leitor.rCampo(tcInt, 'cdResposta');
        Status.descResposta := Leitor.rCampo(tcStr, 'descResposta');
        Status.tempoEstimadoConclusao := Leitor.rCampo(tcInt, 'tempoEstimadoConclusao');

        if leitor.rExtrai(3, 'ocorrencias') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'ocorrencia', '', i + 1) <> '' do
          begin
            Status.Ocorrencias.Add;
            Status.Ocorrencias.Items[i].Codigo      := FLeitor.rCampo(tcInt, 'codigo');
            Status.Ocorrencias.Items[i].Descricao   := FLeitor.rCampo(tcStr, 'descricao');
            Status.Ocorrencias.Items[i].Tipo        := FLeitor.rCampo(tcInt, 'tipo');
            Status.Ocorrencias.Items[i].Localizacao := FLeitor.rCampo(tcStr, 'localizacao');
            inc(i);
          end;
        end;

      end;

      if leitor.rExtrai(2, 'dadosRecepcaoLote') <> '' then
      begin
        dadosRecLote.dhRecepcao          := Leitor.rCampo(tcDatHor, 'dhRecepcao');
        dadosRecLote.versaoAplicRecepcao := FLeitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
        dadosRecLote.Protocolo           := FLeitor.rCampo(tcStr, 'protocoloEnvio');
      end;

      if leitor.rExtrai(2, 'dadosProcessamentoLote') <> '' then
        dadosProcLote.versaoAplicProcLote := FLeitor.rCampo(tcStr, 'versaoAplicativoProcessamentoLote');

      if leitor.rExtrai(2, 'retornoEventos') <> '' then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'evento', '', i + 1) <> '' do
        begin
          RetEventos.Add;
          RetEventos.Items[i].IDEvento := FLeitor.rAtributo('Id=', 'evento');

          if leitor.rExtrai(4, 'recepcao') <> '' then
          begin
            RetEventos.Items[i].Recepcao.tpAmb               := eSStrTotpAmb(Ok, Leitor.rCampo(tcStr, 'tpAmb'));
            RetEventos.Items[i].Recepcao.dhRecepcao          := Leitor.rCampo(tcDatHor, 'dhRecepcao', '');
            RetEventos.Items[i].Recepcao.versaoAplicRecepcao := Leitor.rCampo(tcStr, 'versaoAppRecepcao');
            RetEventos.Items[i].Recepcao.Protocolo           := Leitor.rCampo(tcStr, 'protocoloEnvioLote');
          end;

          if leitor.rExtrai(4, 'processamento') <> '' then
          begin
            RetEventos.Items[i].Processamento.cdResposta          := Leitor.rCampo(tcInt, 'cdResposta');
            RetEventos.Items[i].Processamento.descResposta        := Leitor.rCampo(tcStr, 'descResposta');
            RetEventos.Items[i].Processamento.versaoAplicProcLote := Leitor.rCampo(tcStr, 'versaoAppProcessamento');
            RetEventos.Items[i].Processamento.dhProcessamento     := Leitor.rCampo(tcDatHor, 'dhProcessamento');

            if leitor.rExtrai(5, 'ocorrencias') <> '' then
            begin
              j := 0;
              while Leitor.rExtrai(6, 'ocorrencia', '', j + 1) <> '' do
              begin
                RetEventos.Items[i].Processamento.Ocorrencias.Add;
                RetEventos.Items[i].Processamento.Ocorrencias.Items[j].Codigo      := FLeitor.rCampo(tcInt, 'codigo');
                RetEventos.Items[i].Processamento.Ocorrencias.Items[j].Descricao   := FLeitor.rCampo(tcStr, 'descricao');
                RetEventos.Items[i].Processamento.Ocorrencias.Items[j].Tipo        := FLeitor.rCampo(tcInt, 'tipo');
                RetEventos.Items[i].Processamento.Ocorrencias.Items[j].Localizacao := FLeitor.rCampo(tcStr, 'localizacao');
                inc(j);
              end;
            end;
          end;

          if leitor.rExtrai(4, 'recibo') <> '' then
          begin
            RetEventos.Items[i].Recibo.nrRecibo := Leitor.rCampo(tcStr, 'nrRecibo');
            RetEventos.Items[i].Recibo.Hash     := Leitor.rCampo(tcStr, 'hash');

            // Falta Implementar a estrutura do elemento Contrato.
          end;

          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

