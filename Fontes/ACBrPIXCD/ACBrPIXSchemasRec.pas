{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasRec;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase, ACBrPIXBase,
  ACBrPIXSchemasRecParticipantes,
  ACBrPIXSchemasRecBase,
  ACBrPIXSchemasSolicRec,
  ACBrPIXSchemasLocation,
  ACBrPIXSchemasPaginacao;

type

  { TACBrPIXRecCompleta }

  TACBrPIXRecCompleta = class(TACBrPIXRecBase)
  private
    fLoc: TACBrPIXRecLocation;
    fSolicitacao: TACBrPIXSolicitacaoRecCompleta;
    function GetLoc: TACBrPIXRecLocation;
    function GetSolicitacao: TACBrPIXSolicitacaoRecCompleta;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecCompleta);

    property idRec;
    property vinculo;
    property calendario;
    property valor;
    property recebedor;
    property pagador;
    property status;
    property politicaRetentativa;
    property atualizacao;
    property encerramento;
    property ativacao;
    property dadosQR;

    property loc: TACBrPIXRecLocation read GetLoc;
    property solicitacao: TACBrPIXSolicitacaoRecCompleta read GetSolicitacao;
  end;

  { TACBrPIXRecGerada }

  TACBrPIXRecGerada = class(TACBrPIXRecCompleta)
  public
    property idRec;
    property vinculo;
    property calendario;
    property valor;
    property recebedor;
    property status;
    property loc;
    property atualizacao;
    property encerramento;
    property ativacao;
  end;

  { TACBrPIXRecCompletaLista }

  TACBrPIXRecCompletaLista = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXRecCompleta;
    procedure SetItem(Index: Integer; Value: TACBrPIXRecCompleta);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrPIXRecCompleta): Integer;
    procedure Insert(Index: Integer; aItem: TACBrPIXRecCompleta);
    function New: TACBrPIXRecCompleta;
    property Items[Index: Integer]: TACBrPIXRecCompleta read GetItem write SetItem; default;
  end;

  { TACBrPIXRecSolicitadaBase }

  TACBrPIXRecSolicitadaBase = class(TACBrPIXRecBase)
  private
    fLoc: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property loc: Integer read fLoc write fLoc;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecSolicitadaBase);
  end;

  { TACBrPIXRecSolicitada }

  TACBrPIXRecSolicitada = class(TACBrPIXRecSolicitadaBase)
  public
    property loc;
    property valor;
    property vinculo;
    property recebedor;
    property ativacao;
    property calendario;
    property politicaRetentativa;
  end;

  { TACBrPIXRecRevisada }

  TACBrPIXRecRevisada = class(TACBrPIXRecSolicitadaBase)
  public
    property loc;
    property status;
    property vinculo;
    property ativacao;
    property calendario;
  end;

  { TACBrPIXRecConsultaParametros }

  TACBrPIXRecConsultaParametros = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    flocationPresente: Boolean;
    ffim: TDateTime;
    finicio: TDateTime;
    fpaginacao: TACBrPIXPaginacao;
    frecebedor: TACBrPIXRecSolicRecebedor;
    function GetPaginacao: TACBrPIXPaginacao;
    function GetRecebedor: TACBrPIXRecSolicRecebedor;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecConsultaParametros);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property locationPresente: Boolean read flocationPresente write flocationPresente;
    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property recebedor: TACBrPIXRecSolicRecebedor read GetRecebedor;
    property paginacao: TACBrPIXPaginacao read GetPaginacao;
  end;

  { TACBrPIXRecsConsultadas }

  TACBrPIXRecsConsultadas = class(TACBrPIXSchema)
  private
    frecs: TACBrPIXRecCompletaLista;
    fparametros: TACBrPIXRecConsultaParametros;
    function GetParametros: TACBrPIXRecConsultaParametros;
    function GetRecs: TACBrPIXRecCompletaLista;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXRecsConsultadas);
    function IsEmpty: Boolean; override;
    
    property recs: TACBrPIXRecCompletaLista read GetRecs;
    property parametros: TACBrPIXRecConsultaParametros read GetParametros;
  end;

implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXRecSolicitadaBase }

procedure TACBrPIXRecSolicitadaBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  AJSon.AddPair('loc', fLoc, False);
end;

procedure TACBrPIXRecSolicitadaBase.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  AJSon.Value('loc', fLoc);
end;

procedure TACBrPIXRecSolicitadaBase.Clear;
begin
  inherited Clear;
  fLoc := 0;
end;

function TACBrPIXRecSolicitadaBase.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and EstaZerado(fLoc);
end;

procedure TACBrPIXRecSolicitadaBase.Assign(Source: TACBrPIXRecSolicitadaBase);
begin
  if not Assigned(Source) then
    Exit;
  inherited Assign(Source);
  fLoc := Source.loc;
end;

{ TACBrPIXRecConsultaParametros }

function TACBrPIXRecConsultaParametros.GetPaginacao: TACBrPIXPaginacao;
begin
  if not Assigned(fpaginacao) then
    fpaginacao := TACBrPIXPaginacao.Create('paginacao');
  Result := fpaginacao;
end;

function TACBrPIXRecConsultaParametros.GetRecebedor: TACBrPIXRecSolicRecebedor;
begin
  if not Assigned(fRecebedor) then
    fRecebedor := TACBrPIXRecSolicRecebedor.Create('recebedor');
  Result := fRecebedor;
end;

procedure TACBrPIXRecConsultaParametros.SetCnpj(AValue: String);
begin
  if (Length(AValue) <> 14) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['CNPJ', 14]);
  fcnpj := AValue;
end;

procedure TACBrPIXRecConsultaParametros.SetCpf(AValue: String);
begin
  if (Length(AValue) <> 11) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['CPF', 11]);
  fcpf := AValue;
end;

procedure TACBrPIXRecConsultaParametros.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if NaoEstaZerado(finicio) then
    AJSon.AddPair('inicio', DateTimeToISO8601(finicio));
  if NaoEstaZerado(ffim) then
    AJSon.AddPair('fim', DateTimeToISO8601(ffim));
  if flocationPresente then
    AJSon.AddPair('locationPresente', flocationPresente);
  AJSon.AddPair('cpf', fcpf, False)
       .AddPair('cnpj', fcnpj, False);

  if Assigned(fRecebedor) and (not fRecebedor.IsEmpty) then
    fRecebedor.WriteToJSon(AJSon);
  if Assigned(fpaginacao) and (not fpaginacao.IsEmpty) then
    fpaginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecConsultaParametros.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wInicio, wFim: String;
begin
  {$IFDEF FPC}
  wInicio := EmptyStr;
  wFim := EmptyStr;
  {$ENDIF}
  AJSon.Value('inicio', wInicio)
       .Value('fim', wFim)
       .Value('locationPresente', flocationPresente)
       .Value('cpf', fcpf)
       .Value('cnpj', fcnpj);

  if NaoEstaVazio(wInicio) then
    finicio := ISO8601ToDateTime(wInicio);
  if NaoEstaVazio(wFim) then
    ffim := ISO8601ToDateTime(wFim);

  Recebedor.ReadFromJSon(AJSon);
  Paginacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecConsultaParametros.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecConsultaParametros.Destroy;
begin
  if Assigned(fRecebedor) then
    fRecebedor.Free;
  if Assigned(fpaginacao) then
    fpaginacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecConsultaParametros.Clear;
begin
  fcnpj := EmptyStr;
  fcpf := EmptyStr;
  flocationPresente := False;
  ffim := 0;
  finicio := 0;
  if Assigned(fRecebedor) then
    fRecebedor.Clear;
  if Assigned(fpaginacao) then
    fpaginacao.Clear;
end;

function TACBrPIXRecConsultaParametros.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcnpj) and
            EstaVazio(fcpf) and
            (flocationPresente = False) and
            EstaZerado(ffim) and
            EstaZerado(finicio) and
            ((not Assigned(fRecebedor)) or fRecebedor.IsEmpty) and
            ((not Assigned(fpaginacao)) or fpaginacao.IsEmpty);
end;

procedure TACBrPIXRecConsultaParametros.Assign(Source: TACBrPIXRecConsultaParametros);
begin
  Clear;
  if not Assigned(Source) then
    Exit;

  fcnpj := Source.cnpj;
  fcpf := Source.cpf;
  flocationPresente := Source.locationPresente;
  ffim := Source.fim;
  finicio := Source.inicio;
  Recebedor.Assign(Source.Recebedor);
  Paginacao.Assign(Source.Paginacao);
end;

{ TACBrPIXRecsConsultadas }

function TACBrPIXRecsConsultadas.GetParametros: TACBrPIXRecConsultaParametros;
begin
  if not Assigned(fparametros) then
    fparametros := TACBrPIXRecConsultaParametros.Create('parametros');
  Result := fparametros;
end;

function TACBrPIXRecsConsultadas.GetRecs: TACBrPIXRecCompletaLista;
begin
  if not Assigned(frecs) then
    frecs := TACBrPIXRecCompletaLista.Create('recs');
  Result := frecs;
end;

procedure TACBrPIXRecsConsultadas.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if Assigned(fparametros) and (not fparametros.IsEmpty) then
    fparametros.WriteToJSon(AJSon);
  if Assigned(frecs) and (not frecs.IsEmpty) then
    frecs.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecsConsultadas.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Parametros.ReadFromJSon(AJSon);
  Recs.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecsConsultadas.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecsConsultadas.Destroy;
begin
  if Assigned(fparametros) then
    fparametros.Free;
  if Assigned(frecs) then
    frecs.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecsConsultadas.Clear;
begin
  if Assigned(fparametros) then
    fparametros.Clear;
  if Assigned(frecs) then
    frecs.Clear;
end;

function TACBrPIXRecsConsultadas.IsEmpty: Boolean;
begin
  Result := ((not Assigned(fparametros)) or fparametros.IsEmpty) and
            ((not Assigned(frecs)) or frecs.IsEmpty);
end;

procedure TACBrPIXRecsConsultadas.Assign(Source: TACBrPIXRecsConsultadas);
begin
  Clear;
  if not Assigned(Source) then
    Exit;

  Parametros.Assign(Source.Parametros);
  Recs.Assign(Source.Recs);
end;

{ TACBrPIXRecCompleta }

function TACBrPIXRecCompleta.GetLoc: TACBrPIXRecLocation;
begin
  if (not Assigned(fLoc)) then
    fLoc := TACBrPIXRecLocation.Create('loc');
  Result := fLoc;
end;

function TACBrPIXRecCompleta.GetSolicitacao: TACBrPIXSolicitacaoRecCompleta;
begin
  if (not Assigned(fSolicitacao)) then
    fSolicitacao := TACBrPIXSolicitacaoRecCompleta.Create('solicitacao');
  Result := fSolicitacao;
end;

procedure TACBrPIXRecCompleta.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (Assigned(ASource)) and (ASource is TACBrPIXRecCompleta) then
    Assign(TACBrPIXRecCompleta(ASource));
end;

procedure TACBrPIXRecCompleta.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  if Assigned(fLoc) and (not fLoc.IsEmpty) then
    fLoc.WriteToJSon(AJSon);
  if Assigned(fSolicitacao) and (not fSolicitacao.IsEmpty) then
    fSolicitacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecCompleta.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  if Assigned(fLoc) then
    fLoc.ReadFromJSon(AJSon);
  if Assigned(fSolicitacao) then
    fSolicitacao.ReadFromJSon(AJSon);
end;

destructor TACBrPIXRecCompleta.Destroy;
begin
  if Assigned(fLoc) then
    fLoc.Free;
  if Assigned(fSolicitacao) then
    fSolicitacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecCompleta.Clear;
begin
  inherited Clear;
  if Assigned(fLoc) then
    fLoc.Clear;
  if Assigned(fSolicitacao) then
    fSolicitacao.Clear;
end;

function TACBrPIXRecCompleta.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            ((not Assigned(fLoc)) or fLoc.IsEmpty) and
            ((not Assigned(fSolicitacao)) or fSolicitacao.IsEmpty);
end;

procedure TACBrPIXRecCompleta.Assign(Source: TACBrPIXRecCompleta);
begin
  Clear;
  if not Assigned(Source) then
    Exit;
  inherited Assign(Source);
  loc.Assign(Source.loc);
  solicitacao.Assign(Source.solicitacao);
end;

{ TACBrPIXRecCompletaLista }

function TACBrPIXRecCompletaLista.GetItem(Index: Integer): TACBrPIXRecCompleta;
begin
  Result := TACBrPIXRecCompleta(inherited Items[Index]);
end;

procedure TACBrPIXRecCompletaLista.SetItem(Index: Integer; Value: TACBrPIXRecCompleta);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXRecCompletaLista.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXRecCompletaLista.Add(aItem: TACBrPIXRecCompleta): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPIXRecCompletaLista.Insert(Index: Integer; aItem: TACBrPIXRecCompleta);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXRecCompletaLista.New: TACBrPIXRecCompleta;
begin
  Result := TACBrPIXRecCompleta.Create;
  Add(Result);
end;

end.

