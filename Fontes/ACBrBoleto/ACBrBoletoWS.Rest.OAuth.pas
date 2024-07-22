{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior, Victor H Gonzales - Pandaaa   }
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
unit ACBrBoletoWS.Rest.OAuth;

interface

uses
  pcnConversao,
  ACBrOpenSSLUtils,
  httpsend,
  ACBrBoletoConversao,
  ACBrBoleto,
  ACBrUtil.FilesIO;

type
  TpAuthorizationType = (atNoAuth, atBearer);

  TParams = record
    prName, PrValue: String;
  end;

    { TOAuth }
  TOAuth = class
  private
    FURL              : String;
    FContentType      : String;
    FGrantType        : String;
    FScope            : String;
    FAmbiente         : TpcnTipoAmbiente;
    FClientID         : String;
    FClientSecret     : String;
    FToken            : String;
    FExpire           : TDateTime;
    FErroComunicacao  : String;
    FPayload          : Boolean;
    FHTTPSend         : THTTPSend;
    FParamsOAuth      : string;
    FAuthorizationType: TpAuthorizationType;
    FHeaderParamsList : Array of TParams;
    FACBrBoleto       : TACBrBoleto;
    FArqLOG           : String;
    procedure setURL(const AValue: String);
    procedure setContentType(const AValue: String);
    procedure setGrantType(const AValue: String);
    procedure setPayload(const AValue: Boolean);

    function getURL: String;
    function getContentType: String;
    function getGrantType: String;
    function getClientID: String;
    function getClientSecret: String;
    function getScope: String;
    procedure GravaLog(const AString: AnsiString);
    procedure ProcessarRespostaOAuth(const ARetorno: AnsiString);
    function Executar(const AAuthBase64: String): Boolean;
    procedure SetAuthorizationType(const Value: TpAuthorizationType);
  protected

  public
    constructor Create(ASSL: THTTPSend; AACBrBoleto: TACBrBoleto = nil);
    destructor Destroy; Override;
    property URL: String read getURL write setURL;
    function GerarToken: Boolean;
    property ParamsOAuth: String read FParamsOAuth write FParamsOAuth;
    function AddHeaderParam(AParamName, AParamValue: String): TOAuth;

    function ClearHeaderParams(): TOAuth;
    property ContentType: String read getContentType write setContentType;
    property GrantType: String read getGrantType write setGrantType;
    property Scope: String read getScope;
    property ClientID: String read getClientID;
    property ClientSecret: String read getClientSecret;
    property Ambiente: TpcnTipoAmbiente read FAmbiente default taHomologacao;
    property Expire: TDateTime read FExpire;
    property ErroComunicacao: String read FErroComunicacao;
    property Token: String read FToken;
    property Payload: Boolean read FPayload write setPayload;
    property AuthorizationType: TpAuthorizationType read FAuthorizationType write SetAuthorizationType;
    procedure DoLog(const AString: String; const ANivelSeveridadeLog : TNivelLog);
  end;

implementation

uses
  SysUtils,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrBoletoWS,
  ACBrJSON,
  DateUtils,
  Classes,
  synacode,
  synautil;

  { TOAuth }

procedure TOAuth.setURL(const AValue: String);
begin
  if FURL <> AValue then
    FURL := AValue;
end;

procedure TOAuth.setContentType(const AValue: String);
begin
  if FContentType <> AValue then
    FContentType := AValue;
end;

procedure TOAuth.setGrantType(const AValue: String);
begin
  if FGrantType <> AValue then
    FGrantType := AValue;

end;

procedure TOAuth.setPayload(const AValue: Boolean);
begin
  if FPayload <> AValue then
    FPayload := AValue;
end;

function TOAuth.getURL: String;
begin
  if FURL = '' then
    Raise Exception.Create(ACBrStr('Método de Autenticação inválido. URL não definida!'))
  else
    Result := FURL;
end;

procedure TOAuth.GravaLog(const AString: AnsiString);
begin
  if (FArqLOG = '') then
    Exit;

  WriteLog(FArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) + ' - ' + AString);
end;

function TOAuth.getContentType: String;
begin
  if FContentType = '' then
    Result := 'application/x-www-form-urlencoded'
  else
    Result := FContentType;
end;

function TOAuth.getGrantType: String;
begin
  if FGrantType = '' then
    Result := 'client_credentials'
  else
    Result := FGrantType;
end;

function TOAuth.getClientID: String;
begin
  if FClientID = '' then
    Raise Exception.Create(ACBrStr('Client_ID não Informado'));

  Result := FClientID;
end;

function TOAuth.getClientSecret: String;
begin
  if FClientSecret = '' then
    Raise Exception.Create(ACBrStr('Client_Secret não Informado'));

  Result := FClientSecret;
end;

function TOAuth.getScope: String;
begin
  if FScope = '' then
    Raise Exception.Create(ACBrStr('Scope não Informado'));

  Result := FScope;
end;

procedure TOAuth.ProcessarRespostaOAuth(const ARetorno: AnsiString);
var
  LJson        : TACBrJSONObject;
  LErrorMessage: String;
begin
  FToken           := '';
  FExpire          := 0;
  FErroComunicacao := '';
  try
    LJson := TACBrJSONObject.Parse(UTF8ToNativeString(ARetorno));
    try
      if (FHTTPSend.ResultCode in [ 200 .. 205 ]) then
      begin
        FToken := LJson.AsString[ 'access_token' ];
        try
          FExpire := now + (LJson.AsInteger[ 'expires_in' ] * OneSecond);
          DoLog('Validade: ' + DateTimeToStr(FExpire), logSimples);
        except
          FExpire := 0;
        end;
      end
      else
      begin
        FErroComunicacao := 'HTTP_Code=' + IntToStr(FHTTPSend.ResultCode);
        if Assigned(LJson) then
        begin
          LErrorMessage := LJson.AsString[ 'error_description' ];
          if LErrorMessage = '' then
            LErrorMessage  := LJson.AsString[ 'error_title' ];
          FErroComunicacao := FErroComunicacao + ' Erro=' + LErrorMessage;
        end;
        DoLog('Erro: ' + FErroComunicacao, logSimples);
      end;
    finally
      LJson.Free;
    end;
  except
    FErroComunicacao := 'HTTP_Code=' + IntToStr(FHTTPSend.ResultCode) + ' Erro=' + ARetorno;
    DoLog('Erro: ' + FErroComunicacao, logSimples);
  end;
end;

procedure TOAuth.SetAuthorizationType(const Value: TpAuthorizationType);
begin
  FAuthorizationType := Value;
end;

function TOAuth.Executar(const AAuthBase64: String): Boolean;
var
  LHeaders: TStringList;
  I       : Integer;
begin
  FErroComunicacao := '';

  if not Assigned(FHTTPSend) then
    raise EACBrBoletoWSException.Create(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_DFESSL ]));

  //Definido Valor para Timeout com a configuração da Classe
  FHTTPSend.Timeout := FACBrBoleto.Configuracoes.WebService.TimeOut;

  //Definindo Header da requisição OAuth
  FHTTPSend.Headers.Clear;
  LHeaders := TStringList.Create;
  try
      //LHeaders.Add(C_CONTENT_TYPE  + ': ' + ContentType);
    if Self.AuthorizationType = atBearer then
      LHeaders.Add(C_AUTHORIZATION + ': ' + AAuthBase64);
      //LHeaders.Add(C_CACHE_CONTROL + ': ' + C_NO_CACHE);
    for I := 0 to Length(FHeaderParamsList) - 1 do
      LHeaders.Add(FHeaderParamsList[ I ].prName + ': ' + FHeaderParamsList[ I ].PrValue);
    FHTTPSend.Headers.AddStrings(LHeaders);
  finally
    LHeaders.Free;
  end;

  FHTTPSend.MimeType := ContentType;
  try
    try
        //Utiliza HTTPMethod para envio
      DoLog('Comando Enviar: ' + Self.ClassName, logSimples);
      DoLog('Header Envio:' + FHTTPSend.Headers.Text, logParanoico);


      if FPayload then
      begin
        DoLog('URL: [' + MetodoHTTPToStr(htPOST) +'] '+ URL, logSimples);
        DoLog('Body Envio (Payload):' + FParamsOAuth, logParanoico);

        FHTTPSend.Document.Position := 0;
        WriteStrToStream(FHTTPSend.Document, AnsiString(FParamsOAuth));
        FHTTPSend.HTTPMethod(MetodoHTTPToStr(htPOST), URL);
      end
      else
      begin
        DoLog('URL: [' + MetodoHTTPToStr(htPOST) + '] ' + URL + '?' + FParamsOAuth, logSimples);
        FHTTPSend.HTTPMethod(MetodoHTTPToStr(htPOST), URL + '?' + FParamsOAuth);
      end;

      FHTTPSend.Document.Position := 0;
      ProcessarRespostaOAuth(ReadStrFromStream(FHTTPSend.Document, FHTTPSend.Document.Size));

      Result := true;
    except
      on E: Exception do
      begin
        Result           := False;
        FErroComunicacao := E.Message;
      end;
    end;
  finally
    DoLog('Header Resposta:' + FHTTPSend.Headers.Text, logParanoico);

    FHTTPSend.Document.Position := 0;

    DoLog('Body Resposta (payload):' + ReadStrFromStream(FHTTPSend.Document, FHTTPSend.Document.Size), logParanoico);
    if FErroComunicacao <> '' then
      raise EACBrBoletoWSException.Create(ACBrStr('Falha na Autenticação: ' + FErroComunicacao));
  end;
end;

function TOAuth.AddHeaderParam(AParamName, AParamValue: String): TOAuth;
begin
  Result := Self;
  SetLength(FHeaderParamsList, Length(FHeaderParamsList) + 1);
  FHeaderParamsList[ Length(FHeaderParamsList) - 1 ].prName := AParamName;
  FHeaderParamsList[ Length(FHeaderParamsList) - 1 ].PrValue := AParamValue;
end;

function TOAuth.ClearHeaderParams: TOAuth;
begin
  SetLength(FHeaderParamsList, 0);
  Result := Self;
end;

constructor TOAuth.Create(ASSL: THTTPSend; AACBrBoleto: TACBrBoleto = nil);
begin
  if Assigned(ASSL) then
    FHTTPSend := ASSL;

  FACBrBoleto := AACBrBoleto;

    // adiciona a chave privada
  if NaoEstaVazio(AACBrBoleto.Configuracoes.WebService.ChavePrivada) then
  begin
    if StringIsPEM(AACBrBoleto.Configuracoes.WebService.ChavePrivada) then
      FHTTPSend.Sock.SSL.PrivateKey := ConvertPEMToASN1(AACBrBoleto.Configuracoes.WebService.ChavePrivada)
    else
      FHTTPSend.Sock.SSL.PrivateKey := AACBrBoleto.Configuracoes.WebService.ChavePrivada;
  end
  else
    if NaoEstaVazio(AACBrBoleto.Configuracoes.WebService.ArquivoKEY) then
      FHTTPSend.Sock.SSL.PrivateKeyFile := AACBrBoleto.Configuracoes.WebService.ArquivoKEY;

    // adiciona o certificado
  if NaoEstaVazio(AACBrBoleto.Configuracoes.WebService.Certificado) then
  begin
    if StringIsPEM(AACBrBoleto.Configuracoes.WebService.Certificado) then
      FHTTPSend.Sock.SSL.Certificate := ConvertPEMToASN1(AACBrBoleto.Configuracoes.WebService.Certificado)
    else
      FHTTPSend.Sock.SSL.Certificate := AACBrBoleto.Configuracoes.WebService.Certificado;
  end
  else
    if NaoEstaVazio(AACBrBoleto.Configuracoes.WebService.ArquivoCRT) then
      FHTTPSend.Sock.SSL.CertificateFile := AACBrBoleto.Configuracoes.WebService.ArquivoCRT;

  FAmbiente          := AACBrBoleto.Configuracoes.WebService.Ambiente;
  FClientID          := AACBrBoleto.Cedente.CedenteWS.ClientID;
  FClientSecret      := AACBrBoleto.Cedente.CedenteWS.ClientSecret;
  FScope             := AACBrBoleto.Cedente.CedenteWS.Scope;
  FURL               := '';
  FContentType       := '';
  FGrantType         := '';
  FToken             := '';
  FExpire            := 0;
  FErroComunicacao   := '';
  FPayload           := False;
  FAuthorizationType := atBearer;

  if AACBrBoleto.Configuracoes.Arquivos.NomeArquivoLog = '' then
    AACBrBoleto.Configuracoes.Arquivos.NomeArquivoLog := C_ARQBOLETOWS_LOG;

  if not (DirectoryExists(AACBrBoleto.Configuracoes.Arquivos.PathGravarRegistro)) then
    FArqLOG := AACBrBoleto.Configuracoes.Arquivos.NomeArquivoLog
  else
    FArqLOG := PathWithDelim(AACBrBoleto.Configuracoes.Arquivos.PathGravarRegistro) + ExtractFileName(AACBrBoleto.Configuracoes.Arquivos.NomeArquivoLog);

end;

destructor TOAuth.Destroy;
begin
  inherited Destroy;
end;

procedure TOAuth.DoLog(const AString: String; const ANivelSeveridadeLog : TNivelLog);
var
  Tratado: Boolean;
  LLog : string;
begin
  Tratado := False;

  if ANivelSeveridadeLog = logNenhum then
    Exit;

  LLog := NativeStringToAnsi(AString);
  if Assigned(FACBrBoleto.Configuracoes.Arquivos.OnGravarLog) then
    FACBrBoleto.Configuracoes.Arquivos.OnGravarLog(LLog, Tratado);

  if Tratado or (FACBrBoleto.Configuracoes.Arquivos.LogNivel >= ANivelSeveridadeLog) then
    GravaLog(LLog);
end;

function TOAuth.GerarToken: Boolean;
var
  LToken : String;
  LExpire: TDateTime;
begin

  if (Assigned(FACBrBoleto.OnAntesAutenticar)) then
  begin
    FACBrBoleto.OnAntesAutenticar(LToken, LExpire);
    FToken  := LToken;
    FExpire := LExpire;
  end;

  if (Token <> '') and (CompareDateTime(Expire, now) = 1) then //Token ja gerado e ainda válido
    Result := true
  else //Converte Basic da Autenticação em Base64
    Result := Executar('Basic ' + String(EncodeBase64(AnsiString(ClientID + ':' + ClientSecret))));

  if (Assigned(FACBrBoleto.OnDepoisAutenticar)) then
    FACBrBoleto.OnDepoisAutenticar(Token, Expire);
end;

end.
