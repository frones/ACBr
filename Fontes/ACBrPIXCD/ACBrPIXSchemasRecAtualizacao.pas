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

unit ACBrPIXSchemasRecAtualizacao;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase, ACBrPIXBase;

type

  { TACBrPIXRecAtualizacao }

  TACBrPIXRecAtualizacao = class(TACBrPIXSchema)
  private
    fdata: TDateTime;
    fstatus: TACBrPIXStatusRecorrencia;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecAtualizacao);

    property data: TDateTime read fdata write fdata;
    property status: TACBrPIXStatusRecorrencia read fstatus write fstatus;
  end;

  { TACBrPIXRecHistoricoAtualizacao }

  TACBrPIXRecHistoricoAtualizacao = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXRecAtualizacao;
    procedure SetItem(Index: Integer; Value: TACBrPIXRecAtualizacao);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrPIXRecAtualizacao): Integer;
    procedure Insert(Index: Integer; aItem: TACBrPIXRecAtualizacao);
    function New: TACBrPIXRecAtualizacao;
    property Items[Index: Integer]: TACBrPIXRecAtualizacao read GetItem write SetItem; default;
  end; 

  { TACBrPIXCobRAtualizacao }

  TACBrPIXCobRAtualizacao = class(TACBrPIXSchema)
  private
    fdata: TDateTime;
    fstatus: TACBrPIXStatusRegistroCobranca;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRAtualizacao);

    property data: TDateTime read fdata write fdata;
    property status: TACBrPIXStatusRegistroCobranca read fstatus write fstatus;
  end;

  { TACBrPIXCobRHistoricoAtualizacao }

  TACBrPIXCobRHistoricoAtualizacao = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobRAtualizacao;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobRAtualizacao);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrPIXCobRAtualizacao): Integer;
    procedure Insert(Index: Integer; aItem: TACBrPIXCobRAtualizacao);
    function New: TACBrPIXCobRAtualizacao;
    property Items[Index: Integer]: TACBrPIXCobRAtualizacao read GetItem write SetItem; default;
  end;

  { TACBrPIXCobRTentativaAtualizacao }

  TACBrPIXCobRTentativaAtualizacao = class(TACBrPIXSchema)
  private
    fdata: TDateTime;
    fstatus: TACBrPIXStatusTentativaCobranca;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRTentativaAtualizacao);

    property data: TDateTime read fdata write fdata;
    property status: TACBrPIXStatusTentativaCobranca read fstatus write fstatus;
  end;

  { TACBrPIXCobRHistoricoTentativaAtualizacao }

  TACBrPIXCobRHistoricoTentativaAtualizacao = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobRTentativaAtualizacao;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobRTentativaAtualizacao);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrPIXCobRTentativaAtualizacao): Integer;
    procedure Insert(Index: Integer; aItem: TACBrPIXCobRTentativaAtualizacao);
    function New: TACBrPIXCobRTentativaAtualizacao;
    property Items[Index: Integer]: TACBrPIXCobRTentativaAtualizacao read GetItem write SetItem; default;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXRecAtualizacao }

procedure TACBrPIXRecAtualizacao.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXRecAtualizacao) then
    Assign(TACBrPIXRecAtualizacao(ASource));
end;

procedure TACBrPIXRecAtualizacao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('status', PIXStatusRecorrenciaToString(fstatus), False);
  if NaoEstaZerado(fdata) then
    AJSon.AddPair('data', DateTimeToISO8601(fdata));
end;

procedure TACBrPIXRecAtualizacao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wData, wStatus: String;
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  {$IFDEF FPC}
  wData := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}
  AJSon
    .Value('data', wData)
    .Value('status', wStatus);
  if NaoEstaVazio(wData) then
    fdata := ISO8601ToDateTime(wData);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusRecorrencia(wStatus);
end;

constructor TACBrPIXRecAtualizacao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecAtualizacao.Clear;
begin
  fdata := 0;
  fstatus := strNENHUM;
end;

function TACBrPIXRecAtualizacao.IsEmpty: Boolean;
begin
  Result := EstaZerado(fdata) and (fstatus = strNENHUM);
end;

procedure TACBrPIXRecAtualizacao.Assign(Source: TACBrPIXRecAtualizacao);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fdata := Source.data;
  fstatus := Source.status;
end;

{ TACBrPIXRecHistoricoAtualizacao }

function TACBrPIXRecHistoricoAtualizacao.Add(aItem: TACBrPIXRecAtualizacao): Integer;
begin
  Result := inherited Add(aItem);
end;

function TACBrPIXRecHistoricoAtualizacao.GetItem(Index: Integer): TACBrPIXRecAtualizacao;
begin
  Result := TACBrPIXRecAtualizacao(inherited Items[Index]);
end;

procedure TACBrPIXRecHistoricoAtualizacao.Insert(Index: Integer; aItem: TACBrPIXRecAtualizacao);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXRecHistoricoAtualizacao.New: TACBrPIXRecAtualizacao;
begin
  Result := TACBrPIXRecAtualizacao.Create;
  Add(Result);
end;

function TACBrPIXRecHistoricoAtualizacao.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

procedure TACBrPIXRecHistoricoAtualizacao.SetItem(Index: Integer; Value: TACBrPIXRecAtualizacao);
begin
  inherited Items[Index] := Value;
end;

{ TACBrPIXCobRAtualizacao }

procedure TACBrPIXCobRAtualizacao.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXCobRAtualizacao) then
    Assign(TACBrPIXCobRAtualizacao(ASource));
end;

procedure TACBrPIXCobRAtualizacao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('status', PIXStatusRegistroCobrancaToString(fstatus), False);
  if NaoEstaZerado(fdata) then
    AJSon.AddPair('data', DateTimeToISO8601(fdata));
end;

procedure TACBrPIXCobRAtualizacao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wData, wStatus: String;
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  {$IFDEF FPC}
  wData := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}
  AJSon
    .Value('data', wData)
    .Value('status', wStatus);
  if NaoEstaVazio(wData) then
    fdata := ISO8601ToDateTime(wData);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusRegistroCobranca(wStatus);
end;

constructor TACBrPIXCobRAtualizacao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCobRAtualizacao.Clear;
begin
  fdata := 0;
  fstatus := srcNENHUM;
end;

function TACBrPIXCobRAtualizacao.IsEmpty: Boolean;
begin
  Result := EstaZerado(fdata) and (fstatus = srcNENHUM);
end;

procedure TACBrPIXCobRAtualizacao.Assign(Source: TACBrPIXCobRAtualizacao);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fdata := Source.data;
  fstatus := Source.status;
end;

{ TACBrPIXCobRHistoricoAtualizacao }

function TACBrPIXCobRHistoricoAtualizacao.GetItem(Index: Integer): TACBrPIXCobRAtualizacao;
begin
  Result := TACBrPIXCobRAtualizacao(inherited Items[Index]);
end;

procedure TACBrPIXCobRHistoricoAtualizacao.SetItem(Index: Integer; Value: TACBrPIXCobRAtualizacao);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobRHistoricoAtualizacao.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobRHistoricoAtualizacao.Add(aItem: TACBrPIXCobRAtualizacao): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPIXCobRHistoricoAtualizacao.Insert(Index: Integer; aItem: TACBrPIXCobRAtualizacao);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXCobRHistoricoAtualizacao.New: TACBrPIXCobRAtualizacao;
begin
  Result := TACBrPIXCobRAtualizacao.Create;
  Add(Result);
end;

{ TACBrPIXCobRTentativaAtualizacao }

procedure TACBrPIXCobRTentativaAtualizacao.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXCobRTentativaAtualizacao) then
    Assign(TACBrPIXCobRTentativaAtualizacao(ASource));
end;

procedure TACBrPIXCobRTentativaAtualizacao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('status', PIXStatusTentativaCobrancaToString(fstatus), False);
  if NaoEstaZerado(fdata) then
    AJSon.AddPair('data', DateTimeToISO8601(fdata));
end;

procedure TACBrPIXCobRTentativaAtualizacao.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wData, wStatus: String;
begin
  Clear;
  if (not Assigned(AJSon)) then
    Exit;
  {$IFDEF FPC}
  wData := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}
  AJSon
    .Value('data', wData)
    .Value('status', wStatus);
  if NaoEstaVazio(wData) then
    fdata := ISO8601ToDateTime(wData);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusTentativaCobranca(wStatus);
end;

constructor TACBrPIXCobRTentativaAtualizacao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCobRTentativaAtualizacao.Clear;
begin
  fdata := 0;
  fstatus := steNENHUM;
end;

function TACBrPIXCobRTentativaAtualizacao.IsEmpty: Boolean;
begin
  Result := EstaZerado(fdata) and (fstatus = steNENHUM);
end;

procedure TACBrPIXCobRTentativaAtualizacao.Assign(Source: TACBrPIXCobRTentativaAtualizacao);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fdata := Source.data;
  fstatus := Source.status;
end;

{ TACBrPIXCobRHistoricoTentativaAtualizacao }

function TACBrPIXCobRHistoricoTentativaAtualizacao.GetItem(Index: Integer): TACBrPIXCobRTentativaAtualizacao;
begin
  Result := TACBrPIXCobRTentativaAtualizacao(inherited Items[Index]);
end;

procedure TACBrPIXCobRHistoricoTentativaAtualizacao.SetItem(Index: Integer; Value: TACBrPIXCobRTentativaAtualizacao);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobRHistoricoTentativaAtualizacao.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobRHistoricoTentativaAtualizacao.Add(aItem: TACBrPIXCobRTentativaAtualizacao): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPIXCobRHistoricoTentativaAtualizacao.Insert(Index: Integer; aItem: TACBrPIXCobRTentativaAtualizacao);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXCobRHistoricoTentativaAtualizacao.New: TACBrPIXCobRTentativaAtualizacao;
begin
  Result := TACBrPIXCobRTentativaAtualizacao.Create;
  Add(Result);
end;

end.

