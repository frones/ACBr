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

unit ACBrPIXSchemasRecBase;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase,
  ACBrPIXSchemasRecAtualizacao,
  ACBrPIXSchemasRecParticipantes,
  ACBrPIXSchemasCalendario;

type

  { TACBrPIXRecVinculo }

  TACBrPIXRecVinculo = class(TACBrPIXSchema)
  private
    fcontrato: String;
    fobjeto: String;
    fdevedor: TACBrPIXRecDevedor;
    function GetDevedor: TACBrPIXRecDevedor;
    procedure SetObjeto(AValue: String);
    procedure SetContrato(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecVinculo);

    property objeto: String read fobjeto write SetObjeto;
    property contrato: String read fcontrato write SetContrato;
    property devedor: TACBrPIXRecDevedor read GetDevedor;
  end;

  { TACBrPIXRecValor }

  TACBrPIXRecValor = class(TACBrPIXSchema)
  private
    fvalorRec: Currency;
    fvalorMinimoRecebedor: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecValor);

    property valorRec: Currency read fvalorRec write fvalorRec;
    property valorMinimoRecebedor: Currency read fvalorMinimoRecebedor write fvalorMinimoRecebedor;
  end;

  { TACBrPIXRecRejeicao }

  TACBrPIXRecRejeicao = class(TACBrPIXSchema)
  private
    fcodigo: TACBrPIXCodRejeicao;
    fdescricao: String;
    procedure Setdescricao(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecRejeicao);

    property codigo: TACBrPIXCodRejeicao read fcodigo write fcodigo;
    property descricao: String read fdescricao write Setdescricao;
  end;

  { TACBrPIXRecCancelamento }

  TACBrPIXRecCancelamento = class(TACBrPIXSchema)
  private
    fcodigo: TACBrPIXCodCancelamento;
    fdescricao: String;
    fsolicitante: TACBrPIXSolicitante;
    procedure Setdescricao(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecCancelamento);
    
    property descricao: String read fdescricao write Setdescricao;
    property codigo: TACBrPIXCodCancelamento read fcodigo write fcodigo;
    property solicitante: TACBrPIXSolicitante read fsolicitante write fsolicitante;
  end;

  { TACBrPIXRecEncerramento }

  TACBrPIXRecEncerramento = class(TACBrPIXSchema)
  private
    fCancelamento: TACBrPIXRecCancelamento;
    fRejeicao: TACBrPIXRecRejeicao;
    function GetCancelamento: TACBrPIXRecCancelamento;
    function GetRejeicao: TACBrPIXRecRejeicao;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecEncerramento);

    property rejeicao: TACBrPIXRecRejeicao read GetRejeicao;
    property cancelamento: TACBrPIXRecCancelamento read GetCancelamento;
  end;

  { TACBrPIXRecDadosJornada }

  TACBrPIXRecDadosJornada = class(TACBrPIXSchema)
  private
    ftxid: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecDadosJornada);

    property txid: String read ftxid write ftxid;
  end;

  { TACBrPIXRecDadosQR }

  TACBrPIXRecDadosQR = class(TACBrPIXSchema)
  private
    fjornada: TACBrPIXJornada;
    fpixCopiaECola: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecDadosQR);

    property jornada: TACBrPIXJornada read fjornada write fjornada;
    property pixCopiaECola: String read fpixCopiaECola write fpixCopiaECola;
  end;

  { TACBrPIXRecAtivacao }

  TACBrPIXRecAtivacao = class(TACBrPIXSchema)
  private
    fdadosJornada: TACBrPIXRecDadosJornada;
    ftipoJornada: TACBrPIXJornada;
    function GetDadosJornada: TACBrPIXRecDadosJornada;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecAtivacao);

    property tipoJornada: TACBrPIXJornada read ftipoJornada write ftipoJornada;
    property dadosJornada: TACBrPIXRecDadosJornada read GetDadosJornada;
  end;

  { TACBrPIXRecBase }

  TACBrPIXRecBase = class(TACBrPIXSchema)
  private
    fidRec: String;
    fpoliticaRetentativa: TACBrPIXRetentativa;
    fstatus: TACBrPIXStatusRecorrencia;
    fValor: TACBrPIXRecValor;
    fDadosQR: TACBrPIXRecDadosQR;
    fVinculo: TACBrPIXRecVinculo;
    fPagador: TACBrPIXRecPagador;
    fRecebedor: TACBrPIXRecRecebedor;
    fAtivacao: TACBrPIXRecAtivacao;
    fCalendario: TACBrPIXCalendarioRec;
    fEncerramento: TACBrPIXRecEncerramento;
    fAtualizacao: TACBrPIXRecHistoricoAtualizacao;
    function GetAtivacao: TACBrPIXRecAtivacao;
    function GetAtualizacao: TACBrPIXRecHistoricoAtualizacao;
    function GetCalendario: TACBrPIXCalendarioRec;
    function GetdadosQR: TACBrPIXRecDadosQR;
    function GetEncerramento: TACBrPIXRecEncerramento;
    function GetPagador: TACBrPIXRecPagador;
    function GetRecebedor: TACBrPIXRecRecebedor;
    function GetValor: TACBrPIXRecValor;
    function GetVinculo: TACBrPIXRecVinculo;
    procedure SetIdRec(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property idRec: String read fidRec write SetIdRec;
    property status: TACBrPIXStatusRecorrencia read fstatus write fstatus;
    property politicaRetentativa: TACBrPIXRetentativa read fpoliticaRetentativa write fpoliticaRetentativa;

    property valor: TACBrPIXRecValor read GetValor;
    property dadosQR: TACBrPIXRecDadosQR read GetdadosQR;
    property vinculo: TACBrPIXRecVinculo read GetVinculo;
    property pagador: TACBrPIXRecPagador read GetPagador;
    property recebedor: TACBrPIXRecRecebedor read GetRecebedor;
    property ativacao: TACBrPIXRecAtivacao read GetAtivacao;
    property calendario: TACBrPIXCalendarioRec read GetCalendario;
    property encerramento: TACBrPIXRecEncerramento read GetEncerramento;
    property atualizacao: TACBrPIXRecHistoricoAtualizacao read GetAtualizacao;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecBase);
  end;

implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXRecVinculo }

constructor TACBrPIXRecVinculo.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecVinculo.Destroy;
begin
  if Assigned(fdevedor) then
    fdevedor.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecVinculo.Clear;
begin
  fcontrato := EmptyStr;
  fobjeto := EmptyStr;
  if Assigned(fdevedor) then
    fdevedor.Clear;
end;

function TACBrPIXRecVinculo.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcontrato) and
            EstaVazio(fobjeto) and
            ((not Assigned(fdevedor)) or fdevedor.IsEmpty);
end;

procedure TACBrPIXRecVinculo.Assign(Source: TACBrPIXRecVinculo);
begin
  Clear;
  if not Assigned(Source) then
    Exit;

  fcontrato := Source.contrato;
  fobjeto := Source.objeto;
  Devedor.Assign(Source.Devedor);
end;

function TACBrPIXRecVinculo.GetDevedor: TACBrPIXRecDevedor;
begin
  if not Assigned(fdevedor) then
    fdevedor := TACBrPIXRecDevedor.Create('devedor');
  Result := fdevedor;
end;

procedure TACBrPIXRecVinculo.SetObjeto(AValue: String);
begin
  if fobjeto = AValue then Exit;
  fobjeto := Copy(AValue, 1, 35);
end;

procedure TACBrPIXRecVinculo.SetContrato(AValue: String);
begin
  if fcontrato = AValue then Exit;
  fcontrato := Copy(AValue, 1, 35);
end;

procedure TACBrPIXRecVinculo.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('contrato', fcontrato, False)
    .AddPair('objeto', fobjeto, False);

  if Assigned(fdevedor) and not fdevedor.IsEmpty then
    fdevedor.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecVinculo.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  AJSon
    .Value('contrato', fcontrato)
    .Value('objeto', fobjeto);
  devedor.ReadFromJSon(AJSon);
end;

{ TACBrPIXRecValor }

constructor TACBrPIXRecValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecValor.Clear;
begin
  fvalorRec := 0;
  fvalorMinimoRecebedor := 0;
end;

function TACBrPIXRecValor.IsEmpty: Boolean;
begin
  Result := EstaZerado(fvalorRec) and EstaZerado(fvalorMinimoRecebedor);
end;

procedure TACBrPIXRecValor.Assign(Source: TACBrPIXRecValor);
begin
  Clear;
  if not Assigned(Source) then
    Exit;
  fvalorRec := Source.valorRec;
  fvalorMinimoRecebedor := Source.valorMinimoRecebedor;
end;

procedure TACBrPIXRecValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if NaoEstaZerado(fvalorRec) then
    AJSon.AddPair('valorRec', FormatarValorPIX(fvalorRec));
  if NaoEstaZerado(fvalorMinimoRecebedor) then
    AJSon.AddPair('valorMinimoRecebedor', FormatarValorPIX(fvalorMinimoRecebedor));
end;

procedure TACBrPIXRecValor.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  AJSon
    .Value('valorRec', fvalorRec)
    .Value('valorMinimoRecebedor', fvalorMinimoRecebedor);
end;

{ TACBrPIXRecRejeicao }

procedure TACBrPIXRecRejeicao.Setdescricao(AValue: String);
begin
  if fdescricao = AValue then Exit;
  fdescricao := Copy(AValue, 1, 105);
end;

procedure TACBrPIXRecRejeicao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('codigo', PIXCodRejeicaoToString(fcodigo), False)
    .AddPair('descricao', fdescricao, False);
end;

procedure TACBrPIXRecRejeicao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wCodigo: String;
begin
  {$IFDEF FPC}wCodigo := EmptyStr;{$ENDIF}

  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  AJSon
    .Value('codigo', wCodigo)
    .Value('descricao', fdescricao);

  if NaoEstaVazio(wCodigo) then
    fcodigo := StringToPIXCodRejeicao(wCodigo);
end;

constructor TACBrPIXRecRejeicao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecRejeicao.Clear;
begin
  fcodigo := crjNENHUM;
  fdescricao := EmptyStr;
end;

function TACBrPIXRecRejeicao.IsEmpty: Boolean;
begin
  Result := (fcodigo = crjNENHUM) and EstaVazio(fdescricao);
end;

procedure TACBrPIXRecRejeicao.Assign(Source: TACBrPIXRecRejeicao);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fcodigo := Source.codigo;
  fdescricao := Source.descricao;
end;

{ TACBrPIXRecCancelamento }

procedure TACBrPIXRecCancelamento.Setdescricao(AValue: String);
begin
  if fdescricao = AValue then Exit;
  fdescricao := Copy(AValue, 1, 105);
end;

procedure TACBrPIXRecCancelamento.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('codigo', PIXCodCancelamentoToString(fcodigo), False)
    .AddPair('descricao', fdescricao, False)
    .AddPair('solicitante', PIXSolicitanteToString(fsolicitante), False);
end;

procedure TACBrPIXRecCancelamento.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wCodigo, wSolicitante: String;
begin
  {$IFDEF FPC}
  wCodigo := EmptyStr;
  wSolicitante := EmptyStr;
  {$ENDIF}

  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  AJSon
    .Value('codigo', wCodigo)
    .Value('descricao', fdescricao)
    .Value('solicitante', wSolicitante);

  if NaoEstaVazio(wCodigo) then
    fcodigo := StringToPIXCodCancelamento(wCodigo);

  if NaoEstaVazio(wSolicitante) then
    fsolicitante := StringToPIXSolicitante(wSolicitante);
end;

constructor TACBrPIXRecCancelamento.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecCancelamento.Clear;
begin
  fcodigo := cclNENHUM;
  fdescricao := EmptyStr;
  fsolicitante := sttNENHUM;
end;

function TACBrPIXRecCancelamento.IsEmpty: Boolean;
begin
  Result := (fcodigo = cclNENHUM) and EstaVazio(fdescricao) and (fsolicitante = sttNENHUM);
end;

procedure TACBrPIXRecCancelamento.Assign(Source: TACBrPIXRecCancelamento);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fcodigo := Source.codigo;
  fdescricao := Source.descricao;
  fsolicitante := Source.solicitante;
end;

{ TACBrPIXRecEncerramento }

function TACBrPIXRecEncerramento.GetCancelamento: TACBrPIXRecCancelamento;
begin
  if not Assigned(fCancelamento) then
    fCancelamento := TACBrPIXRecCancelamento.Create('cancelamento');
  Result := fCancelamento;
end;

function TACBrPIXRecEncerramento.GetRejeicao: TACBrPIXRecRejeicao;
begin
  if not Assigned(fRejeicao) then
    fRejeicao := TACBrPIXRecRejeicao.Create('rejeicao');
  Result := fRejeicao;
end;

procedure TACBrPIXRecEncerramento.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if Assigned(fRejeicao) and not fRejeicao.IsEmpty then
    fRejeicao.WriteToJSon(AJSon);
  if Assigned(fCancelamento) and not fCancelamento.IsEmpty then
    fCancelamento.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecEncerramento.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  Rejeicao.ReadFromJSon(AJSon);
  Cancelamento.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecEncerramento.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecEncerramento.Destroy;
begin
  if Assigned(fRejeicao) then
    fRejeicao.Free;
  if Assigned(fCancelamento) then
    fCancelamento.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecEncerramento.Clear;
begin
  if Assigned(fRejeicao) then
    fRejeicao.Clear;
  if Assigned(fCancelamento) then
    fCancelamento.Clear;
end;

function TACBrPIXRecEncerramento.IsEmpty: Boolean;
begin
  Result :=
    ((not Assigned(fRejeicao)) or fRejeicao.IsEmpty) and
    ((not Assigned(fCancelamento)) or fCancelamento.IsEmpty);
end;

procedure TACBrPIXRecEncerramento.Assign(Source: TACBrPIXRecEncerramento);
begin
  Clear;
  if not Assigned(Source) then Exit;
  Rejeicao.Assign(Source.Rejeicao);
  Cancelamento.Assign(Source.Cancelamento);
end;

{ TACBrPIXRecDadosJornada }

procedure TACBrPIXRecDadosJornada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('txid', ftxid, False);
end;

procedure TACBrPIXRecDadosJornada.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  AJSon.Value('txid', ftxid);
end;

constructor TACBrPIXRecDadosJornada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecDadosJornada.Clear;
begin
  ftxid := EmptyStr;
end;

function TACBrPIXRecDadosJornada.IsEmpty: Boolean;
begin
  Result := EstaVazio(ftxid);
end;

procedure TACBrPIXRecDadosJornada.Assign(Source: TACBrPIXRecDadosJornada);
begin
  Clear;
  if not Assigned(Source) then Exit;
  ftxid := Source.txid;
end;

{ TACBrPIXRecDadosQR }

procedure TACBrPIXRecDadosQR.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('jornada', PIXJornadaToString(fjornada), False)
    .AddPair('pixCopiaECola', fpixCopiaECola, False);
end;

procedure TACBrPIXRecDadosQR.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wJornada: String;
begin
  {$IFDEF FPC}wJornada := EmptyStr;{$ENDIF}

  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  AJSon
    .Value('jornada', wJornada)
    .Value('pixCopiaECola', fpixCopiaECola);

  if NaoEstaVazio(wJornada) then
    fjornada := StringToPIXJornada(wJornada);
end;

constructor TACBrPIXRecDadosQR.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecDadosQR.Clear;
begin
  fjornada := jorNENHUM;
  fpixCopiaECola := EmptyStr;
end;

function TACBrPIXRecDadosQR.IsEmpty: Boolean;
begin
  Result := (fjornada = jorNENHUM) and EstaVazio(fpixCopiaECola);
end;

procedure TACBrPIXRecDadosQR.Assign(Source: TACBrPIXRecDadosQR);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fjornada := Source.jornada;
  fpixCopiaECola := Source.pixCopiaECola;
end;

{ TACBrPIXRecAtivacao }

function TACBrPIXRecAtivacao.GetDadosJornada: TACBrPIXRecDadosJornada;
begin
  if not Assigned(fdadosJornada) then
    fdadosJornada := TACBrPIXRecDadosJornada.Create('dadosJornada');
  Result := fdadosJornada;
end;

procedure TACBrPIXRecAtivacao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('tipoJornada', PIXJornadaToString(ftipoJornada), False);
  if Assigned(fdadosJornada) and not fdadosJornada.IsEmpty then
    fdadosJornada.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecAtivacao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wTipoJornada: String;
begin
  {$IFDEF FPC}wTipoJornada := EmptyStr;{$ENDIF}

  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  AJSon.Value('tipoJornada', wTipoJornada);
  if NaoEstaVazio(wTipoJornada) then
    ftipoJornada := StringToPIXJornada(wTipoJornada);
  DadosJornada.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecAtivacao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecAtivacao.Destroy;
begin
  if Assigned(fdadosJornada) then
    fdadosJornada.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecAtivacao.Clear;
begin
  ftipoJornada := jorNENHUM;
  if Assigned(fdadosJornada) then
    fdadosJornada.Clear;
end;

function TACBrPIXRecAtivacao.IsEmpty: Boolean;
begin
  Result := (ftipoJornada = jorNENHUM) and
    ((not Assigned(fdadosJornada)) or fdadosJornada.IsEmpty);
end;

procedure TACBrPIXRecAtivacao.Assign(Source: TACBrPIXRecAtivacao);
begin
  Clear;
  if not Assigned(Source) then Exit;
  ftipoJornada := Source.tipoJornada;
  DadosJornada.Assign(Source.DadosJornada);
end;

{ TACBrPIXRecBase }

function TACBrPIXRecBase.GetAtivacao: TACBrPIXRecAtivacao;
begin
  if not Assigned(fAtivacao) then
    fAtivacao := TACBrPIXRecAtivacao.Create('ativacao');
  Result := fAtivacao;
end;

function TACBrPIXRecBase.GetAtualizacao: TACBrPIXRecHistoricoAtualizacao;
begin
  if not Assigned(fAtualizacao) then
    fAtualizacao := TACBrPIXRecHistoricoAtualizacao.Create('atualizacao');
  Result := fAtualizacao;
end;

function TACBrPIXRecBase.GetCalendario: TACBrPIXCalendarioRec;
begin
  if not Assigned(fCalendario) then
    fCalendario := TACBrPIXCalendarioRec.Create('calendario');
  Result := fCalendario;
end;

function TACBrPIXRecBase.GetdadosQR: TACBrPIXRecDadosQR;
begin
  if not Assigned(fDadosQR) then
    fDadosQR := TACBrPIXRecDadosQR.Create('dadosQR');
  Result := fDadosQR;
end;

function TACBrPIXRecBase.GetEncerramento: TACBrPIXRecEncerramento;
begin
  if not Assigned(fEncerramento) then
    fEncerramento := TACBrPIXRecEncerramento.Create('encerramento');
  Result := fEncerramento;
end;

function TACBrPIXRecBase.GetPagador: TACBrPIXRecPagador;
begin
  if not Assigned(fPagador) then
    fPagador := TACBrPIXRecPagador.Create('pagador');
  Result := fPagador;
end;

function TACBrPIXRecBase.GetRecebedor: TACBrPIXRecRecebedor;
begin
  if not Assigned(fRecebedor) then
    fRecebedor := TACBrPIXRecRecebedor.Create('recebedor');
  Result := fRecebedor;
end;

function TACBrPIXRecBase.GetValor: TACBrPIXRecValor;
begin
  if not Assigned(fValor) then
    fValor := TACBrPIXRecValor.Create('valor');
  Result := fValor;
end;

function TACBrPIXRecBase.GetVinculo: TACBrPIXRecVinculo;
begin
  if not Assigned(fVinculo) then
    fVinculo := TACBrPIXRecVinculo.Create('vinculo');
  Result := fVinculo;
end;

procedure TACBrPIXRecBase.SetIdRec(AValue: String);
var
  s, e: String;
begin
  if (fidRec = AValue) then Exit;

  s := Trim(AValue);
  if NaoEstaVazio(s) and IsBacen then
  begin
    e := ValidarIdRec(s);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fidRec := s;
end;

procedure TACBrPIXRecBase.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXRecBase) then
    Assign(TACBrPIXRecBase(ASource));
end;

procedure TACBrPIXRecBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('idRec', fidRec, False)
    .AddPair('status', PIXStatusRecorrenciaToString(fstatus), False)
    .AddPair('politicaRetentativa', PIXRetentativaToString(fpoliticaRetentativa), False);

  if Assigned(fValor) and (not fValor.IsEmpty) then
    fValor.WriteToJSon(AJSon);
  if Assigned(fDadosQR) and (not fDadosQR.IsEmpty) then
    fDadosQR.WriteToJSon(AJSon);
  if Assigned(fVinculo) and (not fVinculo.IsEmpty) then
    fVinculo.WriteToJSon(AJSon);
  if Assigned(fPagador) and (not fPagador.IsEmpty) then
    fPagador.WriteToJSon(AJSon);
  if Assigned(fRecebedor) and (not fRecebedor.IsEmpty) then
    fRecebedor.WriteToJSon(AJSon);
  if Assigned(fAtivacao) and (not fAtivacao.IsEmpty) then
    fAtivacao.WriteToJSon(AJSon);
  if Assigned(fCalendario) and (not fCalendario.IsEmpty) then
    fCalendario.WriteToJSon(AJSon);
  if Assigned(fEncerramento) and (not fEncerramento.IsEmpty) then
    fEncerramento.WriteToJSon(AJSon);
  if Assigned(fAtualizacao) and (not fAtualizacao.IsEmpty) then
    fAtualizacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wStatus, wPolitica: String;
begin
  {$IFDEF FPC}
  wStatus := EmptyStr;
  wPolitica := EmptyStr;
  {$ENDIF}

  Clear;
  if (not Assigned(AJSon)) then
    Exit;

  AJSon
    .Value('idRec', fidRec)
    .Value('status', wStatus)
    .Value('politicaRetentativa', wPolitica);

  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusRecorrencia(wStatus);
  if NaoEstaVazio(wPolitica) then
    fpoliticaRetentativa := StringToPIXRetentativa(wPolitica);

  Valor.ReadFromJSon(AJSon);
  DadosQR.ReadFromJSon(AJSon);
  Vinculo.ReadFromJSon(AJSon);
  Pagador.ReadFromJSon(AJSon);
  Recebedor.ReadFromJSon(AJSon);
  Ativacao.ReadFromJSon(AJSon);
  Calendario.ReadFromJSon(AJSon);
  Encerramento.ReadFromJSon(AJSon);
  Atualizacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecBase.Destroy;
begin
  if Assigned(fValor) then
    fValor.Free;
  if Assigned(fDadosQR) then
    fDadosQR.Free;
  if Assigned(fVinculo) then
    fVinculo.Free;
  if Assigned(fPagador) then
    fPagador.Free;
  if Assigned(fRecebedor) then
    fRecebedor.Free;
  if Assigned(fAtivacao) then
    fAtivacao.Free;
  if Assigned(fCalendario) then
    fCalendario.Free;
  if Assigned(fEncerramento) then
    fEncerramento.Free;
  if Assigned(fAtualizacao) then
    fAtualizacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXRecBase.Clear;
begin
  fidRec := EmptyStr;
  fstatus := strNENHUM;
  fpoliticaRetentativa := rttNENHUM;
  if Assigned(fValor) then
    fValor.Clear;
  if Assigned(fDadosQR) then
    fDadosQR.Clear;
  if Assigned(fVinculo) then
    fVinculo.Clear;
  if Assigned(fPagador) then
    fPagador.Clear;
  if Assigned(fRecebedor) then
    fRecebedor.Clear;
  if Assigned(fAtivacao) then
    fAtivacao.Clear;
  if Assigned(fCalendario) then
    fCalendario.Clear;
  if Assigned(fEncerramento) then
    fEncerramento.Clear;
  if Assigned(fAtualizacao) then
    fAtualizacao.Clear;
end;

function TACBrPIXRecBase.IsEmpty: Boolean;
begin
  Result := EstaVazio(fidRec) and
            (fstatus = strNENHUM) and
            (fpoliticaRetentativa = rttNENHUM) and
            ((not Assigned(fValor)) or fValor.IsEmpty) and
            ((not Assigned(fDadosQR)) or fDadosQR.IsEmpty) and
            ((not Assigned(fVinculo)) or fVinculo.IsEmpty) and
            ((not Assigned(fPagador)) or fPagador.IsEmpty) and
            ((not Assigned(fRecebedor)) or fRecebedor.IsEmpty) and
            ((not Assigned(fAtivacao)) or fAtivacao.IsEmpty) and
            ((not Assigned(fCalendario)) or fCalendario.IsEmpty) and
            ((not Assigned(fEncerramento)) or fEncerramento.IsEmpty) and
            ((not Assigned(fAtualizacao)) or fAtualizacao.IsEmpty);
end;

procedure TACBrPIXRecBase.Assign(Source: TACBrPIXRecBase);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fidRec := Source.idRec;
  fstatus := Source.status;
  fpoliticaRetentativa := Source.politicaRetentativa;
  Valor.Assign(Source.Valor);
  DadosQR.Assign(Source.DadosQR);
  Vinculo.Assign(Source.Vinculo);
  Pagador.Assign(Source.Pagador);
  Recebedor.Assign(Source.Recebedor);
  Ativacao.Assign(Source.Ativacao);
  Calendario.Assign(Source.Calendario);
  Encerramento.Assign(Source.Encerramento);
  Atualizacao.Assign(Source.Atualizacao);
end;

end.

