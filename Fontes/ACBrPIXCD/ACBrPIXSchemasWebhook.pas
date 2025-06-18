{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias C�sar                                                                }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

(*

  Documenta��o:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasWebhook;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase, ACBrPIXBase, ACBrPIXSchemasPaginacao;

type

  { TACBrPIXWebhook }

  TACBrPIXWebhook = class(TACBrPIXSchema)
  private
    fchave: String;
    fcriacao: TDateTime;
    fwebhookUrl: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;

    property webhookUrl: String read fwebhookUrl write fwebhookUrl;
    property criacao: TDateTime read fcriacao write fcriacao;
    property chave: String read fchave write fchave;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPIXWebhook);
  end;

  { TACBrPIXWebhookLista }

  TACBrPIXWebhookLista = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPIXWebhook;
    procedure SetItem(aIndex: Integer; Value: TACBrPIXWebhook);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aWebhook: TACBrPIXWebhook): Integer;
    procedure Insert(aIndex: Integer; aWebhook: TACBrPIXWebhook);
    function New: TACBrPIXWebhook;
    property Items[aIndex: Integer]: TACBrPIXWebhook read GetItem write SetItem; default;
  end;

  { TACBrPIXWebhookRequest }

  TACBrPIXWebhookRequest = class(TACBrPIXWebhook)
  public
    property webhookUrl;
  end;

  { TACBrPIXWebhookResponse }

  TACBrPIXWebhookResponse = class(TACBrPIXWebhook)
  public
    property webhookUrl;
    property criacao;
  end;

  { TACBrPIXWebhookCobResponse }

  TACBrPIXWebhookCobResponse = class(TACBrPIXWebhook)
  public
    property webhookUrl;
    property criacao;
    property chave;
  end;

  { TACBrPIXWebhookParametros }

  TACBrPIXWebhookParametros = class(TACBrPIXSchema)
  private
    fPaginacao: TACBrPIXPaginacao;
    fFim: TDateTime;
    fInicio: TDateTime;
    function GetPaginacao: TACBrPIXPaginacao;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXWebhookParametros);

    property inicio: TDateTime read fInicio write fInicio;
    property fim: TDateTime read fFim write fFim;
    property paginacao: TACBrPIXPaginacao read GetPaginacao;
  end;

  { TACBrPIXWebhookConsultados }

  TACBrPIXWebhookConsultados = class(TACBrPIXSchema)
  private
    fParametros: TACBrPIXWebhookParametros;
    fWebhooks: TACBrPIXWebhookLista;
    function GetParametros: TACBrPIXWebhookParametros;
    function GetWebhooks: TACBrPIXWebhookLista;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXWebhookConsultados);

    property parametros: TACBrPIXWebhookParametros read GetParametros;
    property webhooks: TACBrPIXWebhookLista read GetWebhooks;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.DateTime;

{ TACBrPIXWebhook }

procedure TACBrPIXWebhook.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrPIXWebhook) then
    Assign(TACBrPIXWebhook(aSource));
end;

procedure TACBrPIXWebhook.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if IsEmpty then
    Exit;

  aJSon
    .AddPair('webhookUrl', fwebhookUrl, False)
    .AddPair('chave', fchave, False);
  if NaoEstaZerado(fcriacao) then
    aJSon.AddPair('criacao', DateTimeToISO8601(fcriacao), False);
end;

procedure TACBrPIXWebhook.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  wCriacao: String;
begin
  Clear;
  {$IFDEF FPC}
  wCriacao := EmptyStr;
  {$ENDIF}

  aJSon
    .Value('webhookUrl', fwebhookUrl)
    .Value('chave', fchave)
    .Value('criacao', wCriacao);

  if NaoEstaVazio(wCriacao) then
    fcriacao := ISO8601ToDateTime(wCriacao);
end;

constructor TACBrPIXWebhook.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TACBrPIXWebhook.Clear;
begin
  fcriacao := 0;
  fchave := EmptyStr;
  fwebhookUrl := EmptyStr;
end;

function TACBrPIXWebhook.IsEmpty: Boolean;
begin
  Result := EstaVazio(fwebhookUrl) and
            EstaZerado(fcriacao) and
            EstaVazio(fchave);
end;

procedure TACBrPIXWebhook.Assign(aSource: TACBrPIXWebhook);
begin
  Clear;
  if not Assigned(aSource) then
    Exit;

  fwebhookUrl := aSource.webhookUrl;
  fcriacao := aSource.criacao;
  fchave := aSource.chave;
end;

{ TACBrPIXWebhookLista }

function TACBrPIXWebhookLista.GetItem(aIndex: Integer): TACBrPIXWebhook;
begin
  Result := TACBrPIXWebhook(inherited Items[aIndex]);
end;

procedure TACBrPIXWebhookLista.SetItem(aIndex: Integer; Value: TACBrPIXWebhook);
begin
  inherited Items[aIndex] := Value;
end;

function TACBrPIXWebhookLista.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXWebhookLista.Add(aWebhook: TACBrPIXWebhook): Integer;
begin
  Result := inherited Add(aWebhook);
end;

procedure TACBrPIXWebhookLista.Insert(aIndex: Integer; aWebhook: TACBrPIXWebhook);
begin
  inherited Insert(aIndex, aWebhook);
end;

function TACBrPIXWebhookLista.New: TACBrPIXWebhook;
begin
  Result := TACBrPIXWebhook.Create;
  Add(Result);
end;

{ TACBrPIXWebhookParametros }

function TACBrPIXWebhookParametros.GetPaginacao: TACBrPIXPaginacao;
begin
  if not Assigned(fPaginacao) then
    fPaginacao := TACBrPIXPaginacao.Create('paginacao');
  Result := fPaginacao;
end;

procedure TACBrPIXWebhookParametros.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if IsEmpty then
    Exit;

  AJSon
    .AddPair('inicio', DateTimeToISO8601(fInicio))
    .AddPair('fim', DateTimeToISO8601(fFim));

  paginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXWebhookParametros.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wInicio, wFim: String;
begin
  Clear;
  {$IFDEF FPC}
  wInicio := EmptyStr;
  wFim := EmptyStr;
  {$ENDIF}

  AJSon
    .Value('inicio', wInicio)
    .Value('fim', wFim);

  if NaoEstaVazio(wInicio) then
    fInicio := ISO8601ToDateTime(wInicio);
  if NaoEstaVazio(wFim) then
    fFim := ISO8601ToDateTime(wFim);

  paginacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXWebhookParametros.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXWebhookParametros.Destroy;
begin
  if Assigned(fPaginacao) then
    fPaginacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXWebhookParametros.Clear;
begin
  fInicio := 0;
  fFim := 0;
  if Assigned(fPaginacao) then
    fPaginacao.Clear;
end;

function TACBrPIXWebhookParametros.IsEmpty: Boolean;
begin
  Result := EstaZerado(fInicio) and
            EstaZerado(fFim) and
            ((not Assigned(fPaginacao)) or fPaginacao.IsEmpty);
end;

procedure TACBrPIXWebhookParametros.Assign(Source: TACBrPIXWebhookParametros);
begin
  Clear;
  if not Assigned(Source) then
    Exit;

  fInicio := Source.inicio;
  fFim := Source.fim;
  paginacao.Assign(Source.paginacao);
end;

{ TACBrPIXWebhookConsultados }

function TACBrPIXWebhookConsultados.GetParametros: TACBrPIXWebhookParametros;
begin
  if not Assigned(fParametros) then
    fParametros := TACBrPIXWebhookParametros.Create('parametros');
  Result := fParametros;
end;

function TACBrPIXWebhookConsultados.GetWebhooks: TACBrPIXWebhookLista;
begin
  if not Assigned(fWebhooks) then
    fWebhooks := TACBrPIXWebhookLista.Create('webhooks');
  Result := fWebhooks;
end;

procedure TACBrPIXWebhookConsultados.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  parametros.WriteToJSon(AJSon);
  webhooks.WriteToJSon(AJSon);
end;

procedure TACBrPIXWebhookConsultados.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  parametros.ReadFromJSon(AJSon);
  webhooks.ReadFromJSon(AJSon);
end;

constructor TACBrPIXWebhookConsultados.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXWebhookConsultados.Destroy;
begin
  if Assigned(fParametros) then
    fParametros.Free;
  if Assigned(fWebhooks) then
    fWebhooks.Free;
  inherited Destroy;
end;

procedure TACBrPIXWebhookConsultados.Clear;
begin
  if Assigned(fParametros) then
    fParametros.Clear;
  if Assigned(fWebhooks) then
    fWebhooks.Clear;
end;

function TACBrPIXWebhookConsultados.IsEmpty: Boolean;
begin
  Result := ((not Assigned(fParametros)) or fParametros.IsEmpty) and
            ((not Assigned(fWebhooks)) or fWebhooks.IsEmpty);
end;

procedure TACBrPIXWebhookConsultados.Assign(Source: TACBrPIXWebhookConsultados);
begin
  Clear;
  if not Assigned(Source) then
    Exit;

  parametros.Assign(Source.parametros);
  webhooks.Assign(Source.webhooks);
end;

end.

