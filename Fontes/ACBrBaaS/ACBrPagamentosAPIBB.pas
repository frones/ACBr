{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - José Junior                                                                }
{ - Antônio Júnior                                                             }
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

{$I ACBr.inc}

unit ACBrPagamentosAPIBB;

interface

uses
  Classes, SysUtils,
  ACBrAPIBase,
  ACBrPagamentosAPI,
  ACBrSchemasPagamentosAPI,
  ACBrSchemasPagamentosAPIBB;

const
  cPagamentoBBParamDevAppKey = 'gw-dev-app-key';
  cPagamentoSolicitacaoBBURLSandbox = 'https://api.hm.bb.com.br/pagamentos-lote/v1';
  cPagamentoSolicitacaoBBURLProducao = 'https://api-ip.bb.com.br/pagamentos-lote/v1';
  cPagamentoConsultaBBURLSandbox = 'https://api.hm.bb.com.br/pagamentos-lote/v1';
  cPagamentoConsultaBBURLProducao = 'https://api-ip.bb.com.br/pagamentos-lote/v1';
  cPagamentoConsultaIDBBURLSandbox = 'https://api.hm.bb.com.br/pagamentos-lote/v1';
  cPagamentoConsultaIDBBURLProducao = 'https://api-ip.bb.com.br/pagamentos-lote/v1';
  cPagamentoBBURLAuthSandbox = 'https://oauth.hm.bb.com.br/oauth/token';
  cPagamentoBBURLAuthProducao = 'https://oauth.bb.com.br/oauth/token';
  cPagamentoBBPathLotesPix = 'lotes-transferencias-pix';
  cPagamentoBBPathLotesDARF = 'lotes-darf-preto-normal';
  cPagamentoBBPathLotesBoletos = 'lotes-boletos'; 
  cPagamentoBBPathLotesGRU = 'lotes-gru';
  cPagamentoBBPathLotesGPS = 'lotes-gps';
  cPagamentoBBPathLotesGuias = 'lotes-guias-codigo-barras';
  cPagamentoBBPathPagamentosGRU = 'pagamentos-gru';
  cPagamentoBBPathsolicitacao = 'solicitacao';
  cPagamentoBBPathGuias = 'guias-codigo-barras';
  cPagamentoBBPathGRU = 'gru';
  cPagamentoBBPathGPS = 'gps';
  cPagamentoBBPathDARFPreto = 'darf-preto';
  cPagamentoBBPathBoletos = 'boletos';

type

  TACBrPagamentosAPIBB = class;

  { TACBrPagamentosAPIBBPagamentos }

  TACBrPagamentosAPIBBPagamentos = class(TACBrPagamentosAPIClass)
  private
    function BB: TACBrPagamentosAPIBB;
  public
    function BoletoSolicitarLotePagamentos: Boolean; override;
    function BoletoConsultarLotePagamentos(const aId: String): Boolean; override;
    function BoletoConsultarPagamentoEspecifico(const aId: String): Boolean; override;

    function GuiaCodigoBarrasSolicitarLotePagamentos: Boolean; override;
    function GuiaCodigoBarrasConsultarLotePagamentos(const aId: String): Boolean; override;
    function GuiaCodigoBarrasConsultarPagamentoEspecifico(const aId: String): Boolean; override;

    function GRUSolicitarPagamentos: Boolean; override;
    function GRUConsultarLotePagamentos(const aId: String): Boolean; override;
    function GRUConsultarPagamentoEspecifico(const aId: String): Boolean; override;

    function DARFSolicitarPagamentos: Boolean; override;
    function DARFConsultarLotePagamentos(const aId: String): Boolean; override;
    function DARFConsultarPagamentoEspecifico(const aId: String): Boolean; override;

    function GPSSolicitarPagamentos: Boolean; override;
    function GPSConsultarLotePagamentos(const aId: String): Boolean; override;
    function GPSConsultarPagamentoEspecifico(const aId: String): Boolean; override;
  end;

  { TACBrPagamentosAPIBB }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPagamentosAPIBB = class(TACBrPagamentosAPIBancoClass)
  private
    fDeveloperApplicationKey: String;
    fScopes: TACBrPagamentosBBScopes;

    function ScopeToString(aScope: TACBrPagamentosBBScope): String;
    function ScopesToString(aScopes: TACBrPagamentosBBScopes): String;
  protected
    constructor Create(AOwner: TComponent); override;
    function CalcularURL: String; override;
    function GetPagamentos: TACBrPagamentosAPIClass; override;

    procedure Autenticar; override;
  public
    property developerApplicationKey: String read fDeveloperApplicationKey write fDeveloperApplicationKey;
  published
    property Scopes: TACBrPagamentosBBScopes read fScopes write fScopes;
  end;

implementation

uses
  synautil, synacode, StrUtils,
  ACBrSocket,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrJSON;

{ TACBrPagamentosAPIBBPagamentos }

function TACBrPagamentosAPIBBPagamentos.BB: TACBrPagamentosAPIBB;
begin
  Result := TACBrPagamentosAPIBB(fpBanco);
end;

function TACBrPagamentosAPIBBPagamentos.BoletoSolicitarLotePagamentos: Boolean;
var
  Body: String;
begin 
  Result := False;
  RegistrarLog('BoletoSolicitarLotePagamentos');

  Body := Trim(LoteBoletosSolicitado.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPagamentosAPIException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['LoteBoletosSolicitado']);

  BB.PrepararHTTP;
  WriteStrToStream(BB.HTTPSend.Document, Body);
  BB.HTTPSend.MimeType := CContentTypeApplicationJSon;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);

  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  BB.URLPathParams.Add(cPagamentoBBPathLotesBoletos);
  RegistrarLog('  Req.Body: ' + sLineBreak + Body, 3);
  try
    BB.HTTPMethod(cHTTPMethodPOST, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_CREATED);
  case BB.HTTPResultCode of
    HTTP_CREATED: LoteBoletosCriado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.BoletoConsultarPagamentoEspecifico(
  const aId: String): Boolean;
begin
  RegistrarLog('BoletoConsultarPagamentoEspecifico(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathBoletos);
  BB.URLPathParams.Add(aId);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except 
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: PagamentoBoletoConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GuiaCodigoBarrasSolicitarLotePagamentos: Boolean;
var
  Body: String;
begin
  Result := False;
  RegistrarLog('GuiaCodigoBarrasSolicitarLotePagamentos');

  Body := Trim(LoteGuiasCodigoBarrasSolicitado.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPagamentosAPIException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['LoteGuiasCodigoBarrasSolicitado']);

  BB.PrepararHTTP;
  WriteStrToStream(BB.HTTPSend.Document, Body);
  BB.HTTPSend.MimeType := CContentTypeApplicationJSon;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  BB.URLPathParams.Add(cPagamentoBBPathLotesGuias);
  RegistrarLog('  Req.Body: ' + sLineBreak + Body, 3);

  try
    BB.HTTPMethod(cHTTPMethodPOST, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_CREATED);
  case BB.HTTPResultCode of
    HTTP_CREATED: LoteGuiasCodigoBarrasCriado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GuiaCodigoBarrasConsultarLotePagamentos(const aId: String): Boolean;
begin
  Result := False;
  RegistrarLog('GuiaCodigoBarrasConsultarLotePagamentos(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathLotesGuias);
  BB.URLPathParams.Add(aId);
  BB.URLPathParams.Add(cPagamentoBBPathsolicitacao);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if (not (e is EACBrHTTPError)) then
      raise e;
  end;
                                                       
  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: LoteGuiasCodigoBarrasConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GuiaCodigoBarrasConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  RegistrarLog('GuiaCodigoBarrasConsultarPagamentoEspecifico(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathGuias);
  BB.URLPathParams.Add(aId);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: PagamentoGuiaCodigoBarrasConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GRUSolicitarPagamentos: Boolean;
var
  Body: String;
begin
  Result := False;
  RegistrarLog('GRUSolicitarPagamentos');

  Body := Trim(LoteGRUSolicitado.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPagamentosAPIException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['LoteGRUSolicitado']);

  BB.PrepararHTTP;
  WriteStrToStream(BB.HTTPSend.Document, Body);
  BB.HTTPSend.MimeType := CContentTypeApplicationJSon;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  BB.URLPathParams.Add(cPagamentoBBPathPagamentosGRU);
  RegistrarLog('  Req.Body: ' + sLineBreak + Body, 3);

  try
    BB.HTTPMethod(cHTTPMethodPOST, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_CREATED);
  case BB.HTTPResultCode of
    HTTP_CREATED: LoteGRUCriado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GRUConsultarLotePagamentos(const aId: String): Boolean;
begin
  Result := False;
  RegistrarLog('GRUConsultarLotePagamentos(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathLotesGRU);
  BB.URLPathParams.Add(aId);
  BB.URLPathParams.Add(cPagamentoBBPathsolicitacao);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if (not (e is EACBrHTTPError)) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: LoteGRUConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GRUConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  RegistrarLog('GRUConsultarPagamentoEspecifico(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathGRU);
  BB.URLPathParams.Add(aId);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: PagamentoGRUConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.DARFSolicitarPagamentos: Boolean;
var
  Body: String;
begin
  Result := False;
  RegistrarLog('DARFSolicitarPagamentos');

  Body := Trim(LoteDARFSolicitado.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPagamentosAPIException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['LoteGRUSolicitado']);

  BB.PrepararHTTP;
  WriteStrToStream(BB.HTTPSend.Document, Body);
  BB.HTTPSend.MimeType := CContentTypeApplicationJSon;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  BB.URLPathParams.Add(cPagamentoBBPathPagamentosGRU);
  RegistrarLog('  Req.Body: ' + sLineBreak + Body, 3);

  try
    BB.HTTPMethod(cHTTPMethodPOST, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_CREATED);
  case BB.HTTPResultCode of
    HTTP_CREATED: LoteDARFCriado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.DARFConsultarLotePagamentos(const aId: String): Boolean;
begin
  Result := False;
  RegistrarLog('DARFConsultarLotePagamentos(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathLotesDARF);
  BB.URLPathParams.Add(aId);
  BB.URLPathParams.Add(cPagamentoBBPathsolicitacao);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if (not (e is EACBrHTTPError)) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: LoteDARFConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.DARFConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  RegistrarLog('DARFConsultarPagamentoEspecifico(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathDARFPreto);
  BB.URLPathParams.Add(aId);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    //HTTP_OK: PagamentoDARFConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GPSSolicitarPagamentos: Boolean;
var
  Body: String;
begin
  Result := False;
  RegistrarLog('GPSSolicitarPagamentos');

  Body := Trim(LoteGPSSolicitado.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPagamentosAPIException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['LoteGPSSSolicitado']);

  BB.PrepararHTTP;
  WriteStrToStream(BB.HTTPSend.Document, Body);
  BB.HTTPSend.MimeType := CContentTypeApplicationJSon;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  BB.URLPathParams.Add(cPagamentoBBPathLotesGPS);
  RegistrarLog('  Req.Body: ' + sLineBreak + Body, 3);

  try
    BB.HTTPMethod(cHTTPMethodPOST, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_CREATED);
  case BB.HTTPResultCode of
    HTTP_CREATED: LoteGPSCriado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GPSConsultarLotePagamentos(const aId: String): Boolean;
begin
  Result := False;
  RegistrarLog('GPSConsultarLotePagamentos(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathLotesGPS);
  BB.URLPathParams.Add(aId);
  BB.URLPathParams.Add(cPagamentoBBPathsolicitacao);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;
  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if (not (e is EACBrHTTPError)) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: LoteGPSConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.GPSConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  RegistrarLog('GPSConsultarPagamentoEspecifico(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathGPS);
  BB.URLPathParams.Add(aId);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: PagamentoGPSConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

function TACBrPagamentosAPIBBPagamentos.BoletoConsultarLotePagamentos(
  const aId: String): Boolean;
begin
  RegistrarLog('BoletoConsultarLotePagamentos(' + aId + ')');

  BB.PrepararHTTP;
  BB.HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ Token);
  BB.URLPathParams.Add(cPagamentoBBPathLotesBoletos);
  BB.URLPathParams.Add(aId);
  BB.URLPathParams.Add(cPagamentoBBPathsolicitacao);
  BB.URLQueryParams.Values['gw-dev-app-key'] := BB.developerApplicationKey;

  try
    BB.HTTPMethod(cHTTPMethodGET, BB.CalcularURL);
  except
    on e: Exception do
    if not (e is EACBrHTTPError) then
      raise e;
  end;

  Result := (BB.HTTPResultCode = HTTP_OK);
  case BB.HTTPResultCode of
    HTTP_OK: LoteBoletosConsultado.AsJSON := BB.HTTPResponse;
    HTTP_UNAUTHORIZED: RespostaErros.OAuthError.AsJSON := BB.HTTPResponse;
  else
    RespostaErros.AsJSON := BB.HTTPResponse;
  end;
end;

{ TACBrPagamentosAPIBB }

function TACBrPagamentosAPIBB.ScopeToString(aScope: TACBrPagamentosBBScope): String;
begin
  Result := EmptyStr;
  case aScope of
    pscLotesRequisicao: Result := 'pagamentos-lote.lotes-requisicao';
    pscTransferenciasInfo: Result := 'pagamentos-lote.transferencias-info';
    pscTransferenciasRequisicao: Result := 'pagamentos-lote.transferencias-requisicao';
    pscCancelarRequisicao: Result := 'pagamentos-lote.cancelar-requisicao';
    pscDevolvidosInfo: Result := 'pagamentos-lote.devolvidos-info';
    pscLotesInfo: Result := 'pagamentos-lote.lotes-info';
    pscGuiasSemCodigoBarrasInfo: Result := 'pagamentos-lote.pagamentos-guias-sem-codigo-barras-info';
    pscPagamentosInfo: Result := 'pagamentos-lote.pagamentos-info';
    pscGuiasSemCodigoBarrasRequisicao: Result := 'pagamentos-lote.pagamentos-guias-sem-codigo-barras-requisicao';
    pscCodigoBarrasInfo: Result := 'pagamentos-lote.pagamentos-codigo-barras-info';
    pscBoletosRequisicao: Result := 'pagamentos-lote.boletos-requisicao';
    pscGuiasCodigoBarrasInfo: Result := 'pagamentos-lote.guias-codigo-barras-info';
    pscGuiasCodigoBarrasRequisicao: Result := 'pagamentos-lote.guias-codigo-barras-requisicao';
    pscTransferenciasPixInfo: Result := 'pagamentos-lote.transferencias-pix-info';
    pscTransferenciasPixRequisicao: Result := 'pagamentos-lote.transferencias-pix-requisicao';
    pscPixInfo: Result := 'pagamentos-lote.pix-info';
    pscBoletosInfo: Result := 'pagamentos-lote.boletos-info';
    pscLancamentosInfo: Result := 'pagamentos-lote.lancamentos-info';
  end;
end;

function TACBrPagamentosAPIBB.ScopesToString(aScopes: TACBrPagamentosBBScopes): String;
var
  i: TACBrPagamentosBBScope;
begin
  Result := EmptyStr;
  for i := Low(TACBrPagamentosBBScope) to High(TACBrPagamentosBBScope) do
    if i in aScopes then
      Result := Result + IfThen(NaoEstaVazio(Result), ' ') + ScopeToString(i);
end;

constructor TACBrPagamentosAPIBB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fScopes := [pscBoletosRequisicao, pscBoletosInfo];
end;

function TACBrPagamentosAPIBB.CalcularURL: String;
begin
  Result := EmptyStr;
  if (not Assigned(fpPagamentosAPI)) then
    Exit;

  if (fpPagamentosAPI.Ambiente = eamProducao) then
    Result := cPagamentoSolicitacaoBBURLProducao
  else if (fpPagamentosAPI.Ambiente = eamHomologacao) then
    Result := cPagamentoSolicitacaoBBURLSandbox;
end;

function TACBrPagamentosAPIBB.GetPagamentos: TACBrPagamentosAPIClass;
begin
  if (not Assigned(fpPagamentos)) then
    fpPagamentos := TACBrPagamentosAPIBBPagamentos.Create(Self);
  Result := fpPagamentos;
end;

procedure TACBrPagamentosAPIBB.Autenticar;
var
  wURL, Body, BasicAutentication: String;
  js: TACBrJSONObject;
  qp: TACBrHTTPQueryParams;
begin
  LimparHTTP;

  if (fpPagamentosAPI.Ambiente = eamProducao) then
    wURL := cPagamentoBBURLAuthProducao
  else
    wURL := cPagamentoBBURLAuthSandbox;

  qp := TACBrHTTPQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['scope'] := ScopesToString(Scopes);
    Body := qp.AsURL;
    WriteStrToStream(HTTPSend.Document, Body);
    HttpSend.MimeType := cContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  BasicAutentication := 'Basic ' + EncodeBase64(ClientID + ':' + ClientSecret);
  HTTPSend.Headers.Add(cHTTPHeaderAuthorization + BasicAutentication);
  HTTPMethod(ChttpMethodPOST, wURL);

  if (HTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(HTTPResponse);
    try
      fpToken := js.AsString['access_token'];
    finally
      js.Free;
    end;
  end;
end;

end.
