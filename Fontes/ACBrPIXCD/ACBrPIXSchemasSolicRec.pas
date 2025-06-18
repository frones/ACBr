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

unit ACBrPIXSchemasSolicRec;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase,
  ACBrPIXSchemasRecBase,
  ACBrPIXSchemasRecAtualizacao,
  ACBrPIXSchemasRecParticipantes,
  ACBrPIXSchemasCalendario;

type

  { TACBrPIXRecPayload }

  TACBrPIXRecPayload = class(TACBrPIXRecBase)
  public
    property idRec;
    property vinculo;
    property calendario;
    property valor;
    property recebedor;
    property politicaRetentativa;
    property atualizacao;
  end;

  { TACBrPIXSolicitacaoRecBase }

  TACBrPIXSolicitacaoRecBase = class(TACBrPIXSchema)
  private
    fidRec: String;
    fidSolicRec: String;
    fstatus: TACBrPIXStatusSolicitacaoRecorrencia;
    fatualizacao: TACBrPIXRecHistoricoAtualizacao;
    fcalendario: TACBrPIXCalendarioRecSolic;
    fdestinatario: TACBrPIXRecDestinatario;
    frecPayload: TACBrPIXRecPayload;
    function GetAtualizacao: TACBrPIXRecHistoricoAtualizacao;
    function GetCalendario: TACBrPIXCalendarioRecSolic;
    function GetDestinatario: TACBrPIXRecDestinatario;
    function GetRecPayload: TACBrPIXRecPayload;
    procedure SetIdRec(AValue: String);
    procedure SetIdSolicRec(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property idSolicRec: String read fidSolicRec write SetIdSolicRec;
    property idRec: String read fidRec write SetIdRec;
    property status: TACBrPIXStatusSolicitacaoRecorrencia read fstatus write fstatus;
    property recPayload: TACBrPIXRecPayload read GetRecPayload;
    property destinatario: TACBrPIXRecDestinatario read GetDestinatario;
    property atualizacao: TACBrPIXRecHistoricoAtualizacao read GetAtualizacao;
    property calendario: TACBrPIXCalendarioRecSolic read GetCalendario;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXSolicitacaoRecBase);
  end; 

  { TACBrPIXSolicitacaoRecCompleta }

  TACBrPIXSolicitacaoRecCompleta = class(TACBrPIXSolicitacaoRecBase)
  public
    property idSolicRec;
    property idRec;
    property status;
    property recPayload;
    property destinatario;
    property atualizacao;
    property calendario;
  end;

  { TACBrPIXSolicitacaoRecSolicitada }

  TACBrPIXSolicitacaoRecSolicitada = class(TACBrPIXSolicitacaoRecBase)
  public
    property idRec;
    property calendario;
    property destinatario;
  end;

  { TACBrPIXSolicitacaoRecRevisada }

  TACBrPIXSolicitacaoRecRevisada = class(TACBrPIXSolicitacaoRecBase)
  public
    property status;
  end;

implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXSolicitacaoRecBase }

function TACBrPIXSolicitacaoRecBase.GetAtualizacao: TACBrPIXRecHistoricoAtualizacao;
begin
  if not Assigned(fatualizacao) then
    fatualizacao := TACBrPIXRecHistoricoAtualizacao.Create('atualizacoes');
  Result := fatualizacao;
end;

function TACBrPIXSolicitacaoRecBase.GetCalendario: TACBrPIXCalendarioRecSolic;
begin
  if not Assigned(fcalendario) then
    fcalendario := TACBrPIXCalendarioRecSolic.Create('calendario');
  Result := fcalendario;
end;

function TACBrPIXSolicitacaoRecBase.GetDestinatario: TACBrPIXRecDestinatario;
begin
  if not Assigned(fdestinatario) then
    fdestinatario := TACBrPIXRecDestinatario.Create('destinatario');
  Result := fdestinatario;
end;

function TACBrPIXSolicitacaoRecBase.GetRecPayload: TACBrPIXRecPayload;
begin
  if not Assigned(frecPayload) then
    frecPayload := TACBrPIXRecPayload.Create('recPayload');
  Result := frecPayload;
end;

procedure TACBrPIXSolicitacaoRecBase.SetIdRec(AValue: String);
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

procedure TACBrPIXSolicitacaoRecBase.SetIdSolicRec(AValue: String);
var
  s, e: String;
begin
  if (fidSolicRec = AValue) then Exit;

  s := Trim(AValue);
  if NaoEstaVazio(s) and IsBacen then
  begin
    e := ValidarIdSolicRec(s);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fidSolicRec := s;
end;

procedure TACBrPIXSolicitacaoRecBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('idRec', fidRec, False)
    .AddPair('idSolicRec', fidSolicRec, False)
    .AddPair('status', PIXStatusSolicitacaoRecorrenciaToString(fstatus), False);

  if Assigned(fatualizacao) and (not fatualizacao.IsEmpty) then
    fatualizacao.WriteToJSon(AJSon);
  if Assigned(fcalendario) and (not fcalendario.IsEmpty) then
    fcalendario.WriteToJSon(AJSon);
  if Assigned(fdestinatario) and (not fdestinatario.IsEmpty) then
    fdestinatario.WriteToJSon(AJSon);
  if Assigned(frecPayload) and (not frecPayload.IsEmpty) then
    frecPayload.WriteToJSon(AJSon);
end;

procedure TACBrPIXSolicitacaoRecBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wStatus: String;
begin
  {$IFDEF FPC}wStatus := EmptyStr;{$ENDIF}
  AJSon
    .Value('idRec', fidRec)
    .Value('idSolicRec', fidSolicRec)
    .Value('status', wStatus);

  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusSolicitacaoRecorrencia(wStatus);

  Atualizacao.ReadFromJSon(AJSon);
  Calendario.ReadFromJSon(AJSon);
  Destinatario.ReadFromJSon(AJSon);
  RecPayload.ReadFromJSon(AJSon);
end;

constructor TACBrPIXSolicitacaoRecBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXSolicitacaoRecBase.Clear;
begin
  fidRec := EmptyStr;
  fidSolicRec := EmptyStr;
  fstatus := ssrNENHUM;
  if Assigned(fatualizacao) then
    fatualizacao.Clear;
  if Assigned(fcalendario) then
    fcalendario.Clear;
  if Assigned(fdestinatario) then
    fdestinatario.Clear;
  if Assigned(frecPayload) then
    frecPayload.Clear;
end;

function TACBrPIXSolicitacaoRecBase.IsEmpty: Boolean;
begin
  Result := EstaVazio(fidRec) and
            EstaVazio(fidSolicRec) and
            (fstatus = ssrNENHUM) and
            ((not Assigned(fatualizacao)) or fatualizacao.IsEmpty) and
            ((not Assigned(fcalendario)) or fcalendario.IsEmpty) and
            ((not Assigned(fdestinatario)) or fdestinatario.IsEmpty) and
            ((not Assigned(frecPayload)) or frecPayload.IsEmpty);
end;

procedure TACBrPIXSolicitacaoRecBase.Assign(Source: TACBrPIXSolicitacaoRecBase);
begin
  Clear;
  if not Assigned(Source) then Exit;
  fidRec := Source.idRec;
  fidSolicRec := Source.idSolicRec;
  fstatus := Source.status;
  Atualizacao.Assign(Source.Atualizacao);
  Calendario.Assign(Source.Calendario);
  Destinatario.Assign(Source.Destinatario);
  RecPayload.Assign(Source.RecPayload);
end;

destructor TACBrPIXSolicitacaoRecBase.Destroy;
begin
  if Assigned(fatualizacao) then
    fatualizacao.Free;
  if Assigned(fcalendario) then
    fcalendario.Free;
  if Assigned(fdestinatario) then
    fdestinatario.Free;
  if Assigned(frecPayload) then
    frecPayload.Free;
  inherited Destroy;
end;

end.

