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

unit pcesRetEnvioLote;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TOcorrenciasCollection = class;
  TOcorrenciasCollectionItem = class;

  TOcorrenciasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOcorrenciasCollectionItem;
    procedure SetItem(Index: Integer; Value: TOcorrenciasCollectionItem);
  public
    constructor create(AOwner: TPersistent);

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

  TRetProcLote = class
  private
    FIdeEmpregador: TIdeEmpregador;
    FIdeTransmissor: TIdeTransmissor;
    FdadosRecLote: TDadosRecepcaoLote;
    FdadosProcLote: TDadosProcLote;
//    FretEventos: TRetEventos;
  protected
//    procedure LerXml(const AXml: string);

  public
    constructor Create;
    destructor Destroy; override;

    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador;
    property IdeTransmissor: TIdeTransmissor read FIdeTransmissor;
    property dadosRecLote: TDadosRecepcaoLote read FdadosRecLote;
    property dadosProcLote: TDadosProcLote read FdadosProcLote;
//    property retEventos: TRetEventos read FretEventos;
 end;

  TRetEnvioLote = class(TPersistent)
  private
    FLeitor: TLeitor;
    FcdResposta: Integer;
    FdescResposta: string;
    FversaoAplicProcLote: string;
    FdhProcessamento: TDateTime;
    FOcorrencias: TOcorrenciasCollection;
    FRetProcLote: TRetProcLote;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;

    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: string read FdescResposta write FdescResposta;
    property versaoAplicProcLote: string read FversaoAplicProcLote write FversaoAplicProcLote;
    property dhProcessamento: TDateTime read FdhProcessamento write FdhProcessamento;

    property Ocorrencias: TOcorrenciasCollection read FOcorrencias write FOcorrencias;
    property RetProcLote: TRetProcLote read FRetProcLote write FRetProcLote;
  end;

implementation

{ TretEnvCTe }

constructor TRetEnvioLote.Create;
begin
  FLeitor := TLeitor.Create;
  FOcorrencias := TOcorrenciasCollection.create(Self);
  FRetProcLote := TRetProcLote.Create;
end;

destructor TRetEnvioLote.Destroy;
begin
  FLeitor.Free;
  FOcorrencias.Free;
  FRetProcLote.Free;

  inherited;
end;

function TRetEnvioLote.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if leitor.rExtrai(1, 'retornoEnvioLoteEventos') <> '' then
    begin
      if leitor.rExtrai(2, 'status') <> '' then
      begin
        FcdResposta   := Leitor.rCampo(tcInt, 'cdResposta');
        FdescResposta := Leitor.rCampo(tcStr, 'descResposta');

        if leitor.rExtrai(3, 'ideEmpregador') <> '' then
        begin
          RetProcLote.IdeEmpregador.TpInsc := eSStrToTpInscricao(Ok, FLeitor.rCampo(tcStr, 'tpInsc'));
          RetProcLote.IdeEmpregador.NrInsc := FLeitor.rCampo(tcStr, 'nrInsc');
        end;

        if leitor.rExtrai(3, 'ideTransmissor') <> '' then
        begin
          RetProcLote.IdeTransmissor.TpInsc := eSStrToTpInscricao(Ok, FLeitor.rCampo(tcStr, 'tpInsc'));
          RetProcLote.IdeTransmissor.NrInsc := FLeitor.rCampo(tcStr, 'nrInsc');
        end;

        if leitor.rExtrai(3, 'dadosRecepcaoLote') <> '' then
        begin
          RetProcLote.dadosRecLote.dhRecepcao := Leitor.rCampo(tcDatHor, 'dhRecepcao');
          RetProcLote.dadosRecLote.versaoAplicRecepcao := FLeitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
          RetProcLote.dadosRecLote.Protocolo := FLeitor.rCampo(tcStr, 'protocoloEnvio');
        end;

        if leitor.rExtrai(3, 'ocorrencias') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'ocorrencia', '', i + 1) <> '' do
          begin
            FOcorrencias.Add;
            FOcorrencias.Items[i].Codigo      := FLeitor.rCampo(tcInt, 'codigo');
            FOcorrencias.Items[i].Descricao   := FLeitor.rCampo(tcStr, 'descricao');
            FOcorrencias.Items[i].Tipo        := FLeitor.rCampo(tcInt, 'tipo');
            FOcorrencias.Items[i].Localizacao := FLeitor.rCampo(tcStr, 'localizacao');
            inc(i);
          end;
        end;

      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TOcorrenciasCollection }

function TOcorrenciasCollection.Add: TOcorrenciasCollectionItem;
begin
  Result := TOcorrenciasCollectionItem(inherited Add());
//  Result.Create;
end;

constructor TOcorrenciasCollection.create(AOwner: TPersistent);
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

{ TRetProcLote }

constructor TRetProcLote.Create;
begin
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FdadosRecLote := TDadosRecepcaoLote.Create;
  FdadosProcLote := TdadosProcLote.Create;
end;

destructor TRetProcLote.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FdadosRecLote.Free;
  FdadosProcLote.Free;
//  FretEventos.Free;

  inherited;
end;

  (*
  TretEvento = class(TCollectionItem)
  private
    FIDEvento: string;
    FRecepcao: TRecepcao;
    FProcessamento: TProcessamento;
    FRecibo: TRecibo;
    FSignature: AnsiString;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Processamento: TProcessamento read FProcessamento;
    property IDEvento: string read FIDEvento write FIDEvento;
    property Recibo: TRecibo read FRecibo;

  end;

  TretEventos = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TretEvento;
    procedure SetItem(Index: Integer; Value: TretEvento);

  public
    function Add: TretEvento;
    property Items[Index: Integer]: TretEvento read GetItem
      write SetItem; default;

  end;
  *)
end.

