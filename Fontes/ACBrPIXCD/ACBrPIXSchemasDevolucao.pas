{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrPIXSchemasDevolucao;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase, ACBrPIXBase;

type

  { TACBrPIXHorario }

  TACBrPIXHorario = class(TACBrPIXSchema)
  private
    fliquidacao: TDateTime;
    fsolicitacao: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXHorario);

    property solicitacao: TDateTime read fsolicitacao write fsolicitacao;
    property liquidacao: TDateTime read fliquidacao write fliquidacao;
  end;

  { TACBrPIXDevolucaoSolicitada }

  TACBrPIXDevolucaoSolicitada = class(TACBrPIXSchema)
  private
    fdescricao: String;
    fnatureza: TACBrPIXNaturezaDevolucao;
    fvalor: Currency;
    procedure SetDescricao(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXDevolucaoSolicitada);

    property valor: Currency read fvalor write fvalor;
    property natureza: TACBrPIXNaturezaDevolucao read fnatureza write fnatureza;
    property descricao: String read fdescricao write SetDescricao;
  end;

  { TACBrPIXDevolucao }

  TACBrPIXDevolucao = class(TACBrPIXDevolucaoSolicitada)
  private
    fhorario: TACBrPIXHorario;
    fid: String;
    fmotivo: String;
    frtrId: String;
    fstatus: TACBrPIXStatusDevolucao;
    procedure Setid(AValue: String);
    procedure Setmotivo(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXDevolucao);

    property id: String read fid write Setid;
    property rtrId: String read frtrId write frtrId;
    property horario: TACBrPIXHorario read fhorario;
    property status: TACBrPIXStatusDevolucao read fstatus write fstatus;
    property motivo: String read fmotivo write Setmotivo;
  end;

  { TACBrPIXDevolucoes }

  TACBrPIXDevolucoes = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXDevolucao;
    procedure SetItem(Index: Integer; Value: TACBrPIXDevolucao);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ADevolucao: TACBrPIXDevolucao): Integer;
    Procedure Insert(Index: Integer; ADevolucao: TACBrPIXDevolucao);
    function New: TACBrPIXDevolucao;
    property Items[Index: Integer]: TACBrPIXDevolucao read GetItem write SetItem; default;
  end;


implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings;

{ TACBrPIXHorario }

constructor TACBrPIXHorario.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXHorario.Clear;
begin
  fsolicitacao := 0;
  fliquidacao := 0;
end;

function TACBrPIXHorario.IsEmpty: Boolean;
begin
  Result := (fsolicitacao = 0) and
            (fliquidacao = 0);
end;

procedure TACBrPIXHorario.Assign(Source: TACBrPIXHorario);
begin
  fsolicitacao := Source.solicitacao;
  fliquidacao := Source.liquidacao;
end;

procedure TACBrPIXHorario.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPairISODateTime('solicitacao', fsolicitacao)
    .AddPairISODateTime('liquidacao', fliquidacao);
end;

procedure TACBrPIXHorario.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .ValueISODateTime('solicitacao', fsolicitacao)
    .ValueISODateTime('liquidacao', fliquidacao);
end;

{ TACBrPIXDevolucaoSolicitada }

constructor TACBrPIXDevolucaoSolicitada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDevolucaoSolicitada.Clear;
begin
  fdescricao := '';
  fnatureza := ndNENHUMA;
  fvalor := 0;
end;

function TACBrPIXDevolucaoSolicitada.IsEmpty: Boolean;
begin
  Result := (fdescricao = '') and
            (fnatureza = ndNENHUMA) and
            (fvalor = 0);
end;

procedure TACBrPIXDevolucaoSolicitada.Assign(Source: TACBrPIXDevolucaoSolicitada
  );
begin
  fdescricao := Source.descricao;
  fnatureza := Source.natureza;
  fvalor := Source.valor;
end;

procedure TACBrPIXDevolucaoSolicitada.SetDescricao(AValue: String);
begin
  if fdescricao = AValue then
    Exit;
  fdescricao := copy(AValue, 1, 140);
end;

procedure TACBrPIXDevolucaoSolicitada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if NaoEstaVazio(fdescricao) then
    AJSon.AddPair('descricao', fdescricao);
  if (fnatureza <> ndNENHUMA) then
    AJSon.AddPair('natureza', PIXNaturezaDevolucaoToString(fnatureza));
  if (fvalor <> 0) then
    AJSon.AddPair('valor', FormatarValorPIX(fvalor));
end;

procedure TACBrPIXDevolucaoSolicitada.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  AJSon
    .Value('descricao', fdescricao)
    .Value('natureza', s)
    .Value('valor', fvalor);
  fnatureza := StringToPIXNaturezaDevolucao(s);
end;

{ TACBrPIXDevolucao }

constructor TACBrPIXDevolucao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fhorario := TACBrPIXHorario.Create('horario');
  Clear;
end;

destructor TACBrPIXDevolucao.Destroy;
begin
  fhorario.Free;
  inherited Destroy;
end;

procedure TACBrPIXDevolucao.Clear;
begin
  inherited Clear;
  fid := '';
  fmotivo := '';
  frtrId := '';
  fstatus := stdNENHUM;
  fhorario.Clear;
end;

function TACBrPIXDevolucao.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fid = '') and
            (fmotivo = '') and
            (frtrId = '') and
            (fstatus = stdNENHUM) and
            fhorario.IsEmpty;
end;

procedure TACBrPIXDevolucao.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXDevolucao) then
    Assign(TACBrPIXDevolucao(ASource));
end;

procedure TACBrPIXDevolucao.Assign(Source: TACBrPIXDevolucao);
begin
  inherited Assign(Source);
  fid := Source.id;
  fmotivo := Source.motivo;
  frtrId := Source.rtrId;
  fstatus := Source.status;
  fhorario.Assign(Source.horario);
end;

procedure TACBrPIXDevolucao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);

  if NaoEstaVazio(fid) then
    AJSon.AddPair('id', fid);
  if NaoEstaVazio(fmotivo) then
    AJSon.AddPair('motivo', fmotivo);
  if NaoEstaVazio(frtrId) then
    AJSon.AddPair('rtrId', frtrId);
  if (fstatus <> stdNENHUM) then
    AJSon.AddPair('status', PIXStatusDevolucaoToString(fstatus));
  fhorario.WriteToJSon(AJSon);
end;

procedure TACBrPIXDevolucao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  AJSon
    .Value('id', fid)
    .Value('motivo', fmotivo)
    .Value('rtrId', frtrId)
    .Value('status', s);

  fstatus := StringToPIXStatusDevolucao(s);
  fhorario.ReadFromJSon(AJSon);
end;

procedure TACBrPIXDevolucao.Setid(AValue: String);
var
  e: String;
begin
  if fid = AValue then
    Exit;

  e := ValidarTxId(AValue, 35, 1);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fId := AValue;
end;

procedure TACBrPIXDevolucao.Setmotivo(AValue: String);
begin
  if fmotivo = AValue then
    Exit;
  fmotivo :=copy(AValue, 1, 140);
end;

{ TACBrPIXDevolucoes }

function TACBrPIXDevolucoes.GetItem(Index: Integer): TACBrPIXDevolucao;
begin
  Result := TACBrPIXDevolucao(inherited Items[Index]);
end;

procedure TACBrPIXDevolucoes.SetItem(Index: Integer; Value: TACBrPIXDevolucao);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXDevolucoes.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXDevolucoes.Add(ADevolucao: TACBrPIXDevolucao): Integer;
begin
  Result := inherited Add(ADevolucao);
end;

procedure TACBrPIXDevolucoes.Insert(Index: Integer; ADevolucao: TACBrPIXDevolucao);
begin
  inherited Insert(Index, ADevolucao);
end;

function TACBrPIXDevolucoes.New: TACBrPIXDevolucao;
begin
  Result := TACBrPIXDevolucao.Create('');
  Self.Add(Result);
end;

end.

