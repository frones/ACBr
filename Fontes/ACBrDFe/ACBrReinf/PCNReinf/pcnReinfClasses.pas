{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes
*******************************************************************************}

unit pcnReinfClasses;

interface

uses
  Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs;

type
  TStatus = class;
  IEventoReinf = Interface;
  TregOcorrsCollection = class;

  { TInscricao }
  TInscricao = class(TPersistent)
  protected
    FTpInsc: TtpInsc;
    FNrInsc: string;
  public
    property TpInsc: TtpInsc read FTpInsc write FTpInsc;
    property NrInsc: string read FNrInsc write FNrInsc;
  end;

  { TideContrib }
  TideContrib = class(TInscricao);

  { TIdeTransmissor }
  TIdeTransmissor = class
  private
    FIdTransmissor: string;
  public
    property IdTransmissor: string read FIdTransmissor write FIdTransmissor;
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

  TOcorrenciasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOcorrenciasCollectionItem;
    procedure SetItem(Index: Integer; Value: TOcorrenciasCollectionItem);
  public
    constructor create(AOwner: TStatus);

    function Add: TOcorrenciasCollectionItem;
    property Items[Index: Integer]: TOcorrenciasCollectionItem read GetItem write SetItem;
  end;

  { TStatus }
  TStatus = class
  private
    FcdStatus: Integer;
    FdescRetorno: string;
    FOcorrencias: TOcorrenciasCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cdStatus: Integer read FcdStatus write FcdStatus;
    property descRetorno: string read FdescRetorno write FdescRetorno;
    property Ocorrencias: TOcorrenciasCollection read FOcorrencias write FOcorrencias;
  end;

  { TIdeContribuinte }
  TIdeContribuinte = class(TInscricao)
  private
    FOrgaoPublico: Boolean;
  public
    property OrgaoPublico: Boolean read FOrgaoPublico write FOrgaoPublico;
  end;

  { TIdeEvento }
  TIdeEvento = class(TPersistent)
  private
    FTpAmb: TtpAmb;
    FProcEmi: TprocEmi;
    FVerProc: string;
  public
    property TpAmb: TtpAmb read FTpAmb write FTpAmb;
    property ProcEmi: TprocEmi read FProcEmi write FProcEmi;
    property VerProc: string read FVerProc write FVerProc;
  end;

  { TIdeEvento1 }
  TIdeEvento1 = class(TPersistent)
  private
    FperApur: string;
  public
    property perApur: string read FperApur write FperApur;
  end;

  { TIdePeriodo }
  TIdePeriodo = class(TPersistent)
  private
    FIniValid: string;
    FFimValid: string;
  public
    property IniValid: string read FIniValid write FIniValid;
    property FimValid: string read FFimValid write FFimValid;
  end;

  { TdadosRecepcaoEvento }
  TdadosRecepcaoEvento = class
  private
    FdhProcessamento: TDateTime;
    FTipoEvento: string;
    FHash: string;
    FIDEvento: string;
  public
    property dhProcessamento: TDateTime read FdhProcessamento write FdhProcessamento;
    property tipoEvento: string read FTipoEvento write FTipoEvento;
    property IDEvento: string read FIDEvento write FIDEvento;
    property Hash: string read FHash write FHash;
  end;
  (*
  { TOcorrencia }
  TOcorrencia = class
  private
    Fdescricao: string;
    Fcodigo: string;
    FlocalizacaoErroAviso: string;
    FTipo: Integer;
  public
    Property tipo: Integer Read FTipo write FTipo;
    Property localizacaoErroAviso: string Read FlocalizacaoErroAviso write FlocalizacaoErroAviso;
    Property codigo: string Read Fcodigo write Fcodigo;
    Property descricao: string Read Fdescricao write Fdescricao;
  end;

  { TOcorrencias }
  TOcorrencias = class(TObjectList)
  private
    function GetItem(Index: Integer): TOcorrencia;
    procedure SetItem(Index: Integer; const Value: TOcorrencia);
  public
    function New: TOcorrencia;
    property Items[Index: Integer]: TOcorrencia read GetItem write SetItem;
  end;
  *)

  TregOcorrsCollectionItem = class(TCollectionItem)
  private
    FtpOcorr: Integer;
    FlocalErroAviso: String;
    FcodResp: String;
    FdscResp: String;
  public
    property tpOcorr: Integer read FtpOcorr write FtpOcorr;
    property localErroAviso: String read FlocalErroAviso write FlocalErroAviso;
    property codResp: String read FcodResp write FcodResp;
    property dscResp: String read FdscResp write FdscResp;
  end;

  { TideStatus }
  TideStatus = class(TPersistent)
  private
    FcdRetorno: String;
    FdescRetorno: string;
    FregOcorrs: TregOcorrsCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    property cdRetorno: String read FcdRetorno write FcdRetorno;
    property descRetorno: string read FdescRetorno write FdescRetorno;
    property regOcorrs: TregOcorrsCollection read FregOcorrs write FregOcorrs;
  end;

  TregOcorrsCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TregOcorrsCollectionItem;
    procedure SetItem(Index: Integer; Value: TregOcorrsCollectionItem);
  public
    constructor create(AOwner: TideStatus);

    function Add: TregOcorrsCollectionItem;
    property Items[Index: Integer]: TregOcorrsCollectionItem read GetItem write SetItem;
  end;

  { TdadosReciboEntrega }
  TdadosReciboEntrega = class
  private
    FnumeroRecibo: string;
  public
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
  end;
  (*
  TEvento = class
  private
    FideContrib: TideContrib;
    FdadosRecepcaoEvento: TdadosRecepcaoEvento;
    FStatus: TStatusEvento;
    FId: string;
    FdadosReciboEntrega: TdadosReciboEntrega;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Id: string read FId write FId;
    property ideContrib: TideContrib read FideContrib;
    property dadosRecepcaoEvento: TdadosRecepcaoEvento read FdadosRecepcaoEvento;
    property Status: TStatusEvento read FStatus;
    property dadosReciboEntrega: TdadosReciboEntrega read FdadosReciboEntrega;
  end;

  { TRetEventos }
  TRetEventos = class(TObjectList)
  private
    function GetItem(Index: Integer): TEvento;
    procedure SetItem(Index: Integer; const Value: TEvento);
  public
    function New: TEvento;
    property Items[Index: Integer]: TEvento read GetItem write SetItem;
  end;
  *)
  IEventoReinf = Interface(IInterface)
    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    function GetEvento: TObject;
    
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
  end;

  TeventoCollectionItem = class(TCollectionItem)
  private
    FId: string;
    FArquivoReinf: string;
    FTipo: string;
    FEvento: IEventoReinf;
  public
    constructor create; reintroduce;
    destructor destroy; overload;

    property Id: string read FId write FId;
    property ArquivoReinf: string read FArquivoReinf write FArquivoReinf;
    property Tipo: string read FTipo write FTipo;
    property Evento: IEventoReinf read FEvento write FEvento;
  end;

  { TEvento }
  TeventoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TeventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TeventoCollectionItem);
  public
    constructor create(AOwner: TPersistent);

    function Add: TeventoCollectionItem;
    property Items[Index: Integer]: TeventoCollectionItem read GetItem write SetItem;
  end;

implementation
(*
{ TStatusEvento }

procedure TStatusEvento.AfterConstruction;
begin
  inherited;
   FOcorrencias := TOcorrencias.Create;
end;

procedure TStatusEvento.BeforeDestruction;
begin
  inherited;
  FOcorrencias.Free;
end;

{ TEvento }

procedure TEvento.AfterConstruction;
begin
  inherited;
  FId := EmptyStr;
  FideContrib := TideContrib.Create;
  FdadosRecepcaoEvento := TdadosRecepcaoEvento.Create;
  FStatus := TStatusEvento.Create;
  FdadosReciboEntrega := TdadosReciboEntrega.Create;
end;

procedure TEvento.BeforeDestruction;
begin
  inherited;
  FideContrib.Free;
  FdadosRecepcaoEvento.Free;
  FStatus.Free;
  FdadosReciboEntrega.Free;
end;

{ TOcorrencias }

function TOcorrencias.GetItem(Index: Integer): TOcorrencia;
begin
  Result := TOcorrencia(Inherited Items[Index]);
end;

function TOcorrencias.New: TOcorrencia;
begin
  Result := TOcorrencia.Create;
  Add(Result);
end;

procedure TOcorrencias.SetItem(Index: Integer; const Value: TOcorrencia);
begin
  Put(Index, Value);
end;

{ TRetEventos }

function TRetEventos.GetItem(Index: Integer): TEvento;
begin
  Result := TEvento(Inherited Items[Index]);
end;

function TRetEventos.New: TEvento;
begin
  Result := TEvento.Create;
  Add(Result);
end;

procedure TRetEventos.SetItem(Index: Integer; const Value: TEvento);
begin
  Put(Index, Value);
end;
*)
{ TeventoCollection }

function TeventoCollection.Add: TeventoCollectionItem;
begin
  Result := TeventoCollectionItem(inherited Add());
end;

constructor TeventoCollection.create(AOwner: TPersistent);
begin
  inherited create(TeventoCollectionItem);
end;

function TeventoCollection.GetItem(
  Index: Integer): TeventoCollectionItem;
begin
  Result := TeventoCollectionItem(Inherited GetItem(Index));
end;

procedure TeventoCollection.SetItem(Index: Integer;
  Value: TeventoCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TeventoCollectionItem }

constructor TeventoCollectionItem.create;
begin
  FId                  := EmptyStr;
//  FideContrib          := TideContrib.Create;
//  FdadosRecepcaoEvento := TdadosRecepcaoEvento.Create;
//  FStatus              := TStatusEvento.Create;
//  FdadosReciboEntrega  := TdadosReciboEntrega.Create;
end;

destructor TeventoCollectionItem.destroy;
begin
//  FideContrib.Free;
//  FdadosRecepcaoEvento.Free;
//  FStatus.Free;
//  FdadosReciboEntrega.Free;
end;

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

{ TideStatus }

constructor TideStatus.Create;
begin
  FregOcorrs := TregOcorrsCollection.create(Self);
end;

destructor TideStatus.Destroy;
begin
  FregOcorrs.Free;
  
  inherited;
end;

{ TregOcorrsCollection }

function TregOcorrsCollection.Add: TregOcorrsCollectionItem;
begin
  Result := TregOcorrsCollectionItem(inherited Add());
end;

constructor TregOcorrsCollection.create(AOwner: TideStatus);
begin
  inherited create(TregOcorrsCollectionItem);
end;

function TregOcorrsCollection.GetItem(
  Index: Integer): TregOcorrsCollectionItem;
begin
  Result := TregOcorrsCollectionItem(Inherited GetItem(Index));
end;

procedure TregOcorrsCollection.SetItem(Index: Integer;
  Value: TregOcorrsCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

end.

