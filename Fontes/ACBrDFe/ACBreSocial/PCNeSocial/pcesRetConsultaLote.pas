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
  pcesCommon, pcesConversaoeSocial;

type
  TOcorrenciasCollection = class;
  TOcorrenciasCollectionItem = class;
  TStatus = class;
  TRetEventosCollection = class;
  TRetEventosCollectionItem = class;
  TRetConsultaLote = class;

  TOcorrenciasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOcorrenciasCollectionItem;
    procedure SetItem(Index: Integer; Value: TOcorrenciasCollectionItem);
  public
    constructor create(AOwner: TStatus);

    function Add: TOcorrenciasCollectionItem;
    property Items[Index: Integer]: TOcorrenciasCollectionItem read GetItem write SetItem;
  end;

  TOcorrenciasCollectionItem = class(TCollectionItem)
  private
    FCodigo: Integer;
    FDescricao: String;
    FTipo: Byte;
    FLocalizacao: String;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Byte read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;
  end;

  TStatus = class
  private
    FcdResposta: Integer;
    FdescResposta: string;
    FtempoEstimadoConclusao: Integer;
    FOcorrencias: TOcorrenciasCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: string read FdescResposta write FdescResposta;
    property tempoEstimadoConclusao: Integer read FtempoEstimadoConclusao write FtempoEstimadoConclusao;
    property Ocorrencias: TOcorrenciasCollection read FOcorrencias write FOcorrencias;
  end;

  TDadosRecepcaoLote = class
  private
    FdhRecepcao: TDateTime;
    FversaoAplicRecepcao: String;
    FProtocolo: String;
  public
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicRecepcao: String read FversaoAplicRecepcao
      write FversaoAplicRecepcao;
    property Protocolo: String read FProtocolo write FProtocolo;
  end;

  TdadosProcLote = class
  private
    FversaoAplicProcLote: String;
  public
    property versaoAplicProcLote: String read FversaoAplicProcLote
      write FversaoAplicProcLote;
  end;

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
  public
    property IDEvento: string read FIDEvento write FIDEvento;
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

{ TOcorrenciasCollection }

function TOcorrenciasCollection.Add: TOcorrenciasCollectionItem;
begin
  Result := TOcorrenciasCollectionItem(inherited Add());
end;

constructor TOcorrenciasCollection.create(AOwner: TStatus);
begin
  inherited create(TOcorrenciasCollectionItem);
end;

function TOcorrenciasCollection.GetItem(
  Index: Integer): TOcorrenciasCollectionItem;
begin
  Result := TOcorrenciasCollectionItem(Inherited GetItem(Index));
end;

procedure TOcorrenciasCollection.SetItem(Index: Integer;
  Value: TOcorrenciasCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TStatus }

constructor TStatus.Create;
begin
  FOcorrencias := TOcorrenciasCollection.create(Self);
end;

destructor TStatus.Destroy;
begin
  FOcorrencias.Free;

  inherited;
end;

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
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if leitor.rExtrai(1, 'retornoEnvioLoteEventos') <> '' then
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
        while Leitor.rExtrai(3, 'eventos', '', i + 1) <> '' do
        begin
          RetEventos.Add;
          RetEventos.Items[i].IDEvento := FLeitor.rAtributo('Id=');
          // Falta terminar a implementação
          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

  (*
  try
    FRetProcLote.retEventos.Clear;
    Leitor.Arquivo := FPRetWS;

    if (FRetProcLote.Status in [200, 201]) then
    begin
      Leitor.Arquivo := Leitor.rExtrai(1, 'retornoEventos');
      i := 0;
      while Leitor.rExtrai(1, 'evento', '', i + 1) <> '' do
      begin
        // recepcao
        Reader := TLeitor.Create;
        try
          Reader.Arquivo := Leitor.Grupo;
          retEvento := FRetProcLote.retEventos.Add;
          retEvento.IDEvento := Leitor.rAtributo('Id', 'evento');
          Reader.Grupo := Reader.rExtrai(1, 'recepcao');
          retEvento.FRecepcao.FtpAmb :=
            TptpAmb(Integer(Leitor.rCampo(tcInt, 'tpAmb')));
          retEvento.FRecepcao.FdhRecepcao :=
            Leitor.rCampo(tcDatHor, 'dhRecepcao', '');
          retEvento.FRecepcao.FversaoAplicRecepcao :=
            Leitor.rCampo(tcStr, 'versaoAppRecepcao');
          retEvento.FRecepcao.FProtocolo :=
            Leitor.rCampo(tcStr, 'protocoloEnvioLote');
          // processamento
          Reader.Grupo := Reader.rExtrai(1, 'processamento');
          retEvento.FProcessamento.FcdResposta :=
            Leitor.rCampo(tcStr, 'cdResposta');
          retEvento.FProcessamento.FdescResposta :=
            UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));
          retEvento.FProcessamento.versaoAplicProcLote :=
            Leitor.rCampo(tcStr, 'versaoAppProcessamento');
          retEvento.FProcessamento.FdhProcessamento :=
            Leitor.rCampo(tcDatHor, 'dhProcessamento');
          // recibo
          Reader.Grupo := Reader.rExtrai(1, 'recibo');
          retEvento.FRecibo.FnrRecibo := Leitor.rCampo(tcStr, 'nrRecibo');
          retEvento.FRecibo.FHash := Leitor.rCampo(tcStr, 'hash');
          Processamento := retEvento.FProcessamento;

          j := 0;
          Reader.Arquivo := Reader.rExtrai(1, 'ocorrencias');
          while Reader.rExtrai(1, 'ocorrencia', '', j + 1) <> '' do
          begin
            Processamento.Ocorrencias.Add;
            Processamento.Ocorrencias.Items[j].xml := Reader.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Arquivo := Reader.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Grupo := Reader.Grupo;
            Processamento.Ocorrencias.Items[j].LerXml;
            inc(j);
          end;
          inc(i);
        finally
          Reader.Free;
        end;
      end;
    end;
  finally
    Leitor.Free;
  end;
  *)
end.

