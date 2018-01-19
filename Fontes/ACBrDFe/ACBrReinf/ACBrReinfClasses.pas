{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

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

unit ACBrReinfClasses;

interface

uses Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs;

type
  { TInscricao }
  TInscricao = class(TPersistent)
  protected
    FTpInsc: tpTpInsc;
    FNrInsc: string;
  public
    property TpInsc: tpTpInsc read FTpInsc write FTpInsc;
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

  { TStatus }
  TStatus = class
  private
    FcdStatus: Integer;
    FdescRetorno: string;
  public
    property cdStatus: Integer read FcdStatus write FcdStatus;
    property descRetorno: string read FdescRetorno write FdescRetorno;
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
    FTpAmb: TpTpAmb;
    FProcEmi: TpProcEmi;
    FVerProc: string;
  public
    property TpAmb: TpTpAmb read FTpAmb write FTpAmb;
    property ProcEmi: TpProcEmi read FProcEmi write FProcEmi;
    property VerProc: string read FVerProc write FVerProc;
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

  { TStatusEvento }
  TStatusEvento = class
  private
    FcdRetorno: Integer;
    FdescRetorno: string;
    FOcorrencias: TOcorrencias;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property cdRetorno: Integer read FcdRetorno write FcdRetorno;
    property descRetorno: string read FdescRetorno write FdescRetorno;
    property Ocorrencias: TOcorrencias read FOcorrencias write FOcorrencias;
  end;

  { TdadosReciboEntrega }
  TdadosReciboEntrega = class
  private
    FnumeroRecibo: string;
  public
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
  end;

  { TEvento }
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

implementation

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

end.

