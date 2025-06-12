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

  { TACBrPIXRecAtualizacaoLista }

  TACBrPIXRecAtualizacaoLista = class(TACBrPIXSchemaArray)
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

{ TACBrPIXRecAtualizacaoLista }

function TACBrPIXRecAtualizacaoLista.Add(aItem: TACBrPIXRecAtualizacao): Integer;
begin
  Result := inherited Add(aItem);
end;

function TACBrPIXRecAtualizacaoLista.GetItem(Index: Integer): TACBrPIXRecAtualizacao;
begin
  Result := TACBrPIXRecAtualizacao(inherited GetItem(Index));
end;

procedure TACBrPIXRecAtualizacaoLista.Insert(Index: Integer; aItem: TACBrPIXRecAtualizacao);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXRecAtualizacaoLista.New: TACBrPIXRecAtualizacao;
begin
  Result := TACBrPIXRecAtualizacao.Create;
  Add(Result);
end;

function TACBrPIXRecAtualizacaoLista.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

procedure TACBrPIXRecAtualizacaoLista.SetItem(Index: Integer; Value: TACBrPIXRecAtualizacao);
begin
  inherited Items[Index] := Value;
end;

end.

