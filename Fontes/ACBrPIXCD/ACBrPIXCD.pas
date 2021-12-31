{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrPIXCD;

interface

uses
  Classes, SysUtils,
  httpsend, ssl_openssl,
  ACBrBase,
  ACBrPIXBase, ACBrPIXQRCodeEstatico,
  ACBrPIXSchemasPixConsultados, ACBrPIXSchemasProblema;

const
  ChttpTimeOutDef = 90000;

  ChttpMethodGET = 'GET';
  ChttpMethodPOST = 'POST';
  ChttpMethodPUT = 'PUT';
  ChttpMethodDELETE = 'DELETE';

  CContentTypeUTF8 = 'charset=utf-8';
  CContentTypeJSon = 'application/json';
  CContentTypeApplicationWwwFormUrlEncoded = 'application/x-www-form-urlencoded';

  ChttpHeaderAuthorization = 'Authorization:';
  ChttpHeaderContentType = 'Content-Type:';
  ChttpHeaderContentEncoding = 'Content-Encoding:';
  ChhtpHeaderAcceptEncoding = 'Accept-Encoding:';
  ChttpContentEncodingCompress: array[0..2] of String = ('gzip', 'compress', 'deflate');

  HTTP_CONTINUE                     = 100;
  HTTP_SWITCHING_PROTOCOLS          = 101;
  HTTP_PROCESSING                   = 102;
  HTTP_OK                           = 200;
  HTTP_CREATED                      = 201;
  HTTP_ACCEPTED                     = 202;
  HTTP_NON_AUTHORITATIVE            = 203;
  HTTP_NO_CONTENT                   = 204;
  HTTP_RESET_CONTENT                = 205;
  HTTP_PARTIAL_CONTENT              = 206;
  HTTP_MULTI_STATUS                 = 207;
  HTTP_MULTIPLE_CHOICES             = 300;
  HTTP_MOVED_PERMANENTLY            = 301;
  HTTP_MOVED_TEMPORARILY            = 302;
  HTTP_SEE_OTHER                    = 303;
  HTTP_NOT_MODIFIED                 = 304;
  HTTP_USE_PROXY                    = 305;
  HTTP_TEMPORARY_REDIRECT           = 307;
  HTTP_BAD_REQUEST                  = 400;
  HTTP_UNAUTHORIZED                 = 401;
  HTTP_PAYMENT_REQUIRED             = 402;
  HTTP_FORBIDDEN                    = 403;
  HTTP_NOT_FOUND                    = 404;
  HTTP_METHOD_NOT_ALLOWED           = 405;
  HTTP_NOT_ACCEPTABLE               = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED= 407;
  HTTP_REQUEST_TIME_OUT             = 408;
  HTTP_CONFLICT                     = 409;
  HTTP_GONE                         = 410;
  HTTP_LENGTH_REQUIRED              = 411;
  HTTP_PRECONDITION_FAILED          = 412;
  HTTP_REQUEST_ENTITY_TOO_LARGE     = 413;
  HTTP_REQUEST_URI_TOO_LARGE        = 414;
  HTTP_UNSUPPORTED_MEDIA_TYPE       = 415;
  HTTP_RANGE_NOT_SATISFIABLE        = 416;
  HTTP_EXPECTATION_FAILED           = 417;
  HTTP_UNPROCESSABLE_ENTITY         = 422;
  HTTP_LOCKED                       = 423;
  HTTP_FAILED_DEPENDENCY            = 424;
  HTTP_INTERNAL_SERVER_ERROR        = 500;
  HTTP_NOT_IMPLEMENTED              = 501;
  HTTP_BAD_GATEWAY                  = 502;
  HTTP_SERVICE_UNAVAILABLE          = 503;
  HTTP_GATEWAY_TIME_OUT             = 504;
  HTTP_VERSION_NOT_SUPPORTED        = 505;
  HTTP_VARIANT_ALSO_VARIES          = 506;
  HTTP_INSUFFICIENT_STORAGE         = 507;
  HTTP_NOT_EXTENDED                 = 510;

resourcestring
  sErroMetodoNaoImplementado = 'Método %s não implementado na Classe %s';
  sErroRecebedorNome = 'Nome do Recebedor não informado';
  sErroRecebedorCidade = 'Cidade do Recebedor não informada';
  sErroPSPNaoAtribuido = 'Componente ACBrPSP não atribuido';
  sErroPSPChavePIX = 'Chave Pix não informada';
  sErroPSPTipoChave = 'Chave Pix inválida';
  sErroHttp = 'Erro HTTP: %d, Metodo: %s, URL: %s';


type

  TACBrPixCD = class;
  TACBrPSP = class;

  EACBrPixHttpException = class(EACBrPixException);

  TACBrPixCDAmbiente = (ambTeste, ambProducao);

  { TACBrPixEndPoint - Classe com comandos básicos, para EndPoints}

  TACBrPixEndPoint = class
  private
    fPSP: TACBrPSP;
    fHTTP: THTTPSend;
    fProblema: TACBrPIXProblema;
  protected
    fpEndPoint: String;
    procedure AtribuirErroHTTPProblema;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    procedure AtribuirJSONProblema(RespostaHttp: String);
    property Problema: TACBrPIXProblema read fProblema;
    property EndPoint: String read fpEndPoint;
  end;

  { TACBrPixEndPointPix - EndPoint /pix }

  TACBrPixEndPointPix = class(TACBrPixEndPoint)
  private
    fPixConsultados: TACBrPIXConsultados;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    procedure ConsultarPixRecebidos(Inicio: TDateTime; Fim: TDateTime;
      const TxId: String = ''; const CpfCnpj: String = '';
      PagAtual: Integer = 0; ItensPorPagina: Integer = 100);

    property PixConsultados: TACBrPIXConsultados read fPixConsultados;
  end;

  TACBrQuandoTransmitirHttp = procedure(var AURL: String; var AMethod: String;
    var HttpBody: String) of object;

  TACBrQuandoReceberRespostaHttp = procedure(const AURL: String; const AMethod: String;
    const HttpResponse: String) of object;


  { TACBrQueryParams }

  TACBrQueryParams = class(TStringList)
  private
    function GetAsURL: String;
  public
    property AsURL: String read GetAsURL;
  end;


  { TACBrPSP - O Componente Base, para os PSPs, deve ser conectado em TACBrPixCD}

  TACBrPSP = class(TACBrComponent)
  private
    fChavePIX: String;
    fPathParams: TStringList;
    fQueryParams: TACBrQueryParams;
    fQuandoReceberRespostaHttp: TACBrQuandoReceberRespostaHttp;
    fQuandoTransmitirHttp: TACBrQuandoTransmitirHttp;
    fClientID: AnsiString;
    fClientSecret: AnsiString;
    fk1, fk2: String;
    fTipoChave: TACBrPIXTipoChave;

    fepPix: TACBrPixEndPointPix;
    fPixCD: TACBrPixCD;
    fHttpSend: THTTPSend;

    function GetClientID: String;
    procedure SetClientID(AValue: String);
    function GetClientSecret: String;
    procedure SetClientSecret(AValue: String);
    procedure SetTipoChave(AValue: TACBrPIXTipoChave);
    procedure VerificarPIXCDAtribuido;
    procedure SetChavePIX(AValue: String);
    procedure SetACBrPixCD(AValue: TACBrPixCD);
  protected
    fpAutenticado: Boolean;
    fpToken: String;
    fpRefereshToken: String;
    fpValidadeToken: TDateTime;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Autenticar; virtual;
    procedure VerificarValidadeToken; virtual;
    procedure RenovarToken; virtual;

    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; virtual;
    procedure ConfigurarHTTP; virtual;
    procedure ConfigurarProxy; virtual;
    procedure ConfigurarTimeOut; virtual;
    procedure ConfigurarHeaders(const Method, AURL: String); virtual;
    procedure ConfigurarAutenticacao(const Method, EndPoint: String); virtual;
    procedure ConfigurarPathParameters(const Method, EndPoint: String); virtual;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); virtual;

    function AcessarEndPoint(const Method, EndPoind: String;
      out ResultCode: Integer; out RespostaHttp: String): Boolean; virtual;
    function CalcularURLEndPoint(const Method, EndPoint: String): String; virtual;
    function CalcularEndPointPath(const Method, EndPoint: String): String; virtual;

    function TransmitirHttp(const Method, URL: String;
      out ResultCode: Integer; out RespostaHttp: String): Boolean; virtual;

    property ClientID: String read GetClientID write SetClientID;
    property ClientSecret: String read GetClientSecret write SetClientSecret;

    property QueryParams: TACBrQueryParams read fQueryParams;
    property PathParams: TStringList read fPathParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    procedure VerificarAutenticacao; virtual;
    property Autenticado: Boolean read fpAutenticado;
    property ValidadeToken: TDateTime read fpValidadeToken;

    property epPix: TACBrPixEndPointPix read fepPix;
    property Http: THTTPSend read fHttpSend;
  published
    property ChavePIX: String read fChavePIX write SetChavePIX;
    property TipoChave: TACBrPIXTipoChave read fTipoChave write SetTipoChave stored false;

    property ACBrPixCD: TACBrPixCD read fPixCD write SetACBrPixCD;

    property QuandoTransmitirHttp : TACBrQuandoTransmitirHttp
      read fQuandoTransmitirHttp write fQuandoTransmitirHttp;
    property QuandoReceberRespostaHttp: TACBrQuandoReceberRespostaHttp
      read fQuandoReceberRespostaHttp write fQuandoReceberRespostaHttp;
  end;

  { TACBrPixRecebedor }

  TACBrPixRecebedor = class
  private
    fCEP: String;
    fCidade: String;
    fCodCategoriaComerciante: Integer;
    fNome: String;

    procedure SetCEP(AValue: String);
    procedure SetCidade(AValue: String);
    procedure SetCodCategoriaComerciante(AValue: Integer);
    procedure SetNome(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPixRecebedor);
  published
    property Nome: String read fNome write SetNome;
    property Cidade: String read fCidade write SetCidade;
    property CEP: String read fCEP write SetCEP;
    property CodCategoriaComerciante: Integer read fCodCategoriaComerciante  // https://classification.codes/classifications/industry/mcc/
      write SetCodCategoriaComerciante;
  end;

  { TACBrHttpProxy }

  TACBrHttpProxy = class
  private
    fHost: String;
    fPass: String;
    fPort: String;
    fUser: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrHttpProxy);
  published
    property Host: String read fHost write fHost;
    property Port: String read fPort write fPort;
    property User: String read fUser write fUser;
    property Pass: String read fPass write fPass;
  end;

  { TACBrPixCD - O Componente em si...}

  TACBrPixCD = class(TACBrComponent)
  private
    fAmbiente: TACBrPixCDAmbiente;
    fArqLOG: String;
    fNivelLog: Byte;
    fProxy: TACBrHttpProxy;
    fPSP: TACBrPSP;
    fQuandoGravarLog: TACBrGravarLog;
    fRecebedor: TACBrPixRecebedor;
    fTimeOut: Integer;

    procedure SetACBrPSP(AValue: TACBrPSP);
    procedure SetProxy(AValue: TACBrHttpProxy);
    procedure SetRecebedor(AValue: TACBrPixRecebedor);

    procedure VerificarPSPAtribuido;
    procedure GravarLogEmArquivo(const ALinha: String) ;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegistrarLog(const ALinha: String; NivelLog: Byte = 0);
    procedure DispararExcecao(E: Exception);

    function GerarQRCodeEstatico(Valor: Currency; infoAdicional: String = ''; TxId: String = ''): String;
  published
    property Recebedor: TACBrPixRecebedor read fRecebedor write SetRecebedor;
    property Proxy: TACBrHttpProxy read fProxy write SetProxy;
    property TimeOut: Integer read fTimeOut write fTimeOut default ChttpTimeOutDef;
    property Ambiente: TACBrPixCDAmbiente read fAmbiente write fAmbiente default ambTeste;

    property PSP: TACBrPSP read fPSP write SetACBrPSP;

    property ArqLOG: String read fArqLOG write fArqLOG;
    property NivelLog: Byte read fNivelLog write fNivelLog default 1;
    property QuandoGravarLog: TACBrGravarLog read fQuandoGravarLog write fQuandoGravarLog;
  end;

function URLComDelimitador(AURL: String): String;
function URLSemDelimitador(AURL: String): String;
function GetHeaderValue(const AValue : String; AStringList: TStringList) : String ;
function ContentIsCompressed(AHeader: TStringList): Boolean;

implementation

uses
  StrUtils,
  synacode, synautil,
  ACBrUtil, ACBrCompress, ACBrValidador,
  ACBrPIXUtil;

function URLComDelimitador(AURL: String): String;
begin
  Result := Trim(AURL);
  if (RightStr(Result, 1) <> '/') then
    Result := Result + '/'
end;

function URLSemDelimitador(AURL: String): String;
begin
  Result := Trim(AURL);
  while (Result <> '') and (RightStr(Result, 1) = '/') do
    Delete(Result, Length(Result), 1);
end;

function GetHeaderValue(const AValue : String; AStringList: TStringList) : String ;
var
  i: Integer;
  u, LinhaHeader: String;
begin
  Result := '';
  u := UpperCase(Trim(AValue));
  if (u = '') then
    Exit;

  i := 0;
  while (Result = '') and (i < AStringList.Count) do
  begin
    LinhaHeader := AStringList[i];
    if (pos(u, UpperCase(LinhaHeader)) = 1) then
      Result := Trim(copy(LinhaHeader, Length(u)+1, Length(LinhaHeader)));
    Inc(i);
  end;
end;

function ContentIsCompressed(AHeader: TStringList): Boolean;
var
  ce: String;
  i: Integer;
begin
  ce := GetHeaderValue(ChttpHeaderContentEncoding, AHeader);
  i := Low(ChttpContentEncodingCompress);
  Result := False;
  while (not Result) and (i <= High(ChttpContentEncodingCompress)) do
    Result := (pos(ChttpContentEncodingCompress[i], ce) > 0);
end;

function DecompressStream(AStream: TStream): String;
var
  zt: TCompressType;
begin
  zt := DetectCompressType(AStream);
  if (zt = ctUnknown) then  // Not compressed...
  begin
    AStream.Position := 0;
    Result := ReadStrFromStream(AStream, AStream.Size);
  end
  else
    Result := UnZip(AStream);
end;

{ TACBrPixEndPoint }

constructor TACBrPixEndPoint.Create(AOwner: TACBrPSP);
begin
  inherited Create;
  fPSP := AOwner;
  fHTTP := fPSP.Http;
  fpEndPoint := '';
end;

destructor TACBrPixEndPoint.Destroy;
begin
  fProblema.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPoint.Clear;
begin
  fProblema.Clear;
end;

procedure TACBrPixEndPoint.AtribuirJSONProblema(RespostaHttp: String);
begin
  fProblema.Clear;
  if (Trim(RespostaHttp) = '') then
    AtribuirErroHTTPProblema
  else
  begin
    try
      fProblema.AsJSON := RespostaHttp;
    except
      AtribuirErroHTTPProblema;
    end;
  end;
end;

procedure TACBrPixEndPoint.AtribuirErroHTTPProblema;
begin
  if (fProblema.status = 0) then
    fProblema.status := fHTTP.ResultCode;

  if (fProblema.detail = '') then
  begin
    fHTTP.Document.Position := 0;
    fProblema.detail := ReadStrFromStream(fHTTP.Document, fHTTP.Document.Size);
  end;

  if (fProblema.title = '') then
    fProblema.title := fHTTP.MimeType;
end;

{ TACBrPixEndPointPix }

constructor TACBrPixEndPointPix.Create(AOwner: TACBrPSP);
begin
  if (AOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fPixConsultados := TACBrPIXConsultados.Create;
  fpEndPoint := '/pix';
end;

destructor TACBrPixEndPointPix.Destroy;
begin
  fPixConsultados.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointPix.Clear;
begin
  inherited Clear;
  fPixConsultados.Clear;
end;

procedure TACBrPixEndPointPix.ConsultarPixRecebidos(Inicio: TDateTime;
  Fim: TDateTime; const TxId: String; const CpfCnpj: String; PagAtual: Integer;
  ItensPorPagina: Integer);
var
  RespostaHttp, s, e: String;
  ResultCode: Integer;
begin
  Clear;

  with fPSP.QueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(Inicio);
    Values['fim'] := DateTimeToIso8601(Fim);

    s := Trim(TxId);
    if (s <> '') then
    begin
      Values['txid'] := s;
      Values['txIdPresente'] := 'true';
    end;

    s := OnlyNumber(CpfCnpj);
    if (s <> '') then
    begin
      e := ValidarCNPJouCPF(s);
      if (e <> '') then
        raise EACBrPixException.Create(ACBrStr(e));

      if Length(s) < 12 then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    if (PagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(PagAtual);

    if (ItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(ItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
    fPixConsultados.AsJSON := RespostaHttp
  else
    AtribuirJSONProblema(RespostaHttp);
end;

{ TACBrQueryParams }

function TACBrQueryParams.GetAsURL: String;
var
  i: Integer;
  AName, AValue: String;
begin
  Result := '';
  if (Count = 0) then
    Exit;

  for i := 0 to Count-1 do
  begin
    AName := Names[i];
    if (AName <> '') then
    begin
      Result := Result + IfThen(Result = '','?','&');
      AValue := Values[AName];
      Result := Result + EncodeURLElement(AName)+'='+EncodeURLElement(AValue);
    end;
  end;
end;

{ TACBrPSP }

constructor TACBrPSP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fChavePIX := '';
  fTipoChave := tchNenhuma;
  fpAutenticado := False;
  fpValidadeToken := 0;
  fpToken := '';
  fpRefereshToken := '';

  fHttpSend := THTTPSend.Create;
  fepPix := TACBrPixEndPointPix.Create(Self);
  fQueryParams := TACBrQueryParams.Create;
  fPathParams := TStringList.Create;

  fQuandoTransmitirHttp := Nil;
  fQuandoReceberRespostaHttp := Nil;
end;

destructor TACBrPSP.Destroy;
begin
  fHttpSend.Free;
  fepPix.Free;
  fQueryParams.Free;
  fPathParams.Free;

  inherited Destroy;
end;

procedure TACBrPSP.Clear;
begin
  fHttpSend.Clear;
  fQueryParams.Clear;
  fepPix.Clear;
end;

procedure TACBrPSP.SetACBrPixCD(AValue: TACBrPixCD);
var
  va: TACBrPixCD;
begin
  if (AValue = fPixCD) then
    Exit;

  if Assigned(fPixCD) then
    fPixCD.RemoveFreeNotification(Self);

  va := fPixCD;       // Usa outra variavel para evitar Loop Infinito,
  fPixCD := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.PSP) then
      va.PSP := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.PSP := Self;
  end ;
end;

procedure TACBrPSP.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation) ;
  if (Operation = opRemove) and (fPixCD <> Nil) and (AComponent = fPixCD) then
    fPixCD := Nil
end;

procedure TACBrPSP.VerificarPIXCDAtribuido;
begin
  if not Assigned(fPixCD) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));
end;

function TACBrPSP.GetClientID: String;
begin
  Result := StrCrypt(fClientID, fk1);
end;

procedure TACBrPSP.SetClientID(AValue: String);
begin
  if (fk1 <> '') and (fClientID = StrCrypt(AValue, fk1)) then
    Exit;

  fk1 := FormatDateTime('hhnnsszzz',Now);
  fClientID := StrCrypt(AValue, fk1);  // Salva de forma Criptografada, para evitar "Inspect"
end;

function TACBrPSP.GetClientSecret: String;
begin
  Result := StrCrypt(fClientSecret, fk2);
end;

procedure TACBrPSP.SetClientSecret(AValue: String);
begin
  if (fk2 <> '') and (fClientSecret = StrCrypt(AValue, fk2)) then
    Exit;

  fk2 := FormatDateTime('hhnnsszzz',Now);
  fClientSecret := StrCrypt(AValue, fk2);  // Salva de forma Criptografada, para evitar "Inspect"
end;

procedure TACBrPSP.SetTipoChave(AValue: TACBrPIXTipoChave);
begin
  {}
end;

procedure TACBrPSP.SetChavePIX(AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  if (fChavePix = AValue) then
    Exit;

  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tchNenhuma) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroChaveInvalida), [AValue]);

  fChavePix := Trim(AValue);
  fTipoChave := TipoChave;
end;

procedure TACBrPSP.ConfigurarHTTP;
begin
  Clear;
  ConfigurarProxy;
  ConfigurarTimeOut;
end;

procedure TACBrPSP.ConfigurarProxy;
begin
  VerificarPIXCDAtribuido;
  fHttpSend.ProxyHost := fPixCD.Proxy.Host;
  fHttpSend.ProxyPort := fPixCD.Proxy.Port;
  fHttpSend.ProxyUser := fPixCD.Proxy.User;
  fHttpSend.ProxyPass := fPixCD.Proxy.Pass;
end;

procedure TACBrPSP.ConfigurarTimeOut;
begin
  VerificarPIXCDAtribuido;
  if (fPixCD.TimeOut = 0) then
    Exit;

  fHttpSend.Timeout := fPixCD.TimeOut;
  with fHttpSend.Sock do
  begin
    ConnectionTimeout := fPixCD.TimeOut;
    InterPacketTimeout := False;
    NonblockSendTimeout := fPixCD.TimeOut;
    SocksTimeout := fPixCD.TimeOut;
    HTTPTunnelTimeout := fPixCD.TimeOut;
  end;
end;

procedure TACBrPSP.ConfigurarHeaders(const Method, AURL: String);
var
  i: Integer;
  ae: String;
begin
   { Sobreescrever no PSP, se necessário }

  // Adicionando Header de compactação
  ae := '';
  for i := Low(ChttpContentEncodingCompress) to High(ChttpContentEncodingCompress) do
    ae := ae + ', '+ChttpContentEncodingCompress[i];
  Delete(ae,1,1);

  fHttpSend.Headers.Add(ChhtpHeaderAcceptEncoding + ae);
end;

procedure TACBrPSP.ConfigurarAutenticacao(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
  if (fpToken <> '') then
    fHttpSend.Headers.Insert(0, ChttpHeaderAuthorization + 'Bearer '+fpToken);
end;

procedure TACBrPSP.ConfigurarPathParameters(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
end;

procedure TACBrPSP.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
end;

function TACBrPSP.AcessarEndPoint(const Method, EndPoind: String; out
  ResultCode: Integer; out RespostaHttp: String): Boolean;
var
  AURL: String;
begin
  VerificarPIXCDAtribuido;
  VerificarAutenticacao;

  ConfigurarAutenticacao(Method, EndPoind);
  AURL := CalcularURLEndPoint(Method, EndPoind);

  Result := TransmitirHttp(Method, AURL, ResultCode, RespostaHttp);
end;

function TACBrPSP.CalcularURLEndPoint(const Method, EndPoint: String): String;
var
  AEndPointPath: String;
  i: Integer;
begin
  AEndPointPath := CalcularEndPointPath(Method, EndPoint);
  Result := URLSemDelimitador(ObterURLAmbiente(fPixCD.Ambiente));

  if (AEndPointPath <> '') then
    Result := Result + URLSemDelimitador(AEndPointPath);

  ConfigurarQueryParameters(Method, EndPoint);
  if (fPathParams.Count > 0) then
  begin
    for i := 0 to fPathParams.Count-1 do
      Result := Result + '/' + URLSemDelimitador(EncodeURLElement(fPathParams[i]));
  end;

  Result := Result + fQueryParams.AsURL;
end;

function TACBrPSP.CalcularEndPointPath(const Method, EndPoint: String): String;
begin
  { Sobreescreva em PSPs que usem Nomes de EndPoint, fora do padrão da API do BC }
  Result := Trim(EndPoint);
end;

function TACBrPSP.TransmitirHttp(const Method, URL: String; out
  ResultCode: Integer; out RespostaHttp: String): Boolean;

  function GetHttpBody: String;
  begin
    Result := ReadStrFromStream(fHttpSend.Document, fHttpSend.Document.Size);
  end;

var
  AMethod, AURL, VarBody, HttpBody: String;
begin
  VerificarPIXCDAtribuido;
  HttpBody := '';
  AMethod := Method;
  AURL := URL;

  ConfigurarHTTP;
  ConfigurarHeaders(Method, AURL);

  if Assigned(fQuandoTransmitirHttp) then
  begin
    HttpBody := GetHttpBody;
    VarBody := HttpBody;
    fQuandoTransmitirHttp(AURL, AMethod, VarBody);
    if (HttpBody <> VarBody) then
    begin
      HttpBody := VarBody;
      WriteStrToStream(fHttpSend.Document, HttpBody);
    end;
  end;

  fPixCD.RegistrarLog(AMethod+' '+AURL);
  if (fPixCD.NivelLog > 1) then
  begin
    if (HttpBody = '') then
      HttpBody := GetHttpBody;

    fPixCD.RegistrarLog('Request Headers:'+ sLineBreak + fHttpSend.Headers.Text);
    fPixCD.RegistrarLog('Request Body:'+ sLineBreak + HttpBody);
  end;

  Result := fHttpSend.HTTPMethod(AMethod, AURL);
  ResultCode := fHttpSend.ResultCode;
  if ContentIsCompressed(fHttpSend.Headers) then
    RespostaHttp := DecompressStream(fHttpSend.Document)
  else
    RespostaHttp := ReadStrFromStream(fHttpSend.Document, fHttpSend.Document.Size);

  fPixCD.RegistrarLog('Result Code: '+IntToStr(ResultCode)+' - '+fHttpSend.ResultString);
  if (fPixCD.NivelLog > 1) then
  begin
    fPixCD.RegistrarLog('Response Headers:'+ sLineBreak + fHttpSend.Headers.Text);
    fPixCD.RegistrarLog('Response Body:'+ sLineBreak + RespostaHttp);
  end;

  if Assigned(fQuandoReceberRespostaHttp) then
    fQuandoReceberRespostaHttp(AURL, AMethod, RespostaHttp);
end;

procedure TACBrPSP.Autenticar;
begin
  fpAutenticado := True;
end;

procedure TACBrPSP.VerificarValidadeToken;
begin
  if (ValidadeToken <> 0) and (ValidadeToken < Now) then
    RenovarToken;
end;

procedure TACBrPSP.RenovarToken;
begin
  Autenticar;
end;

function TACBrPSP.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  raise EACBrPixException.Create(
    ACBrStr(Format(sErroMetodoNaoImplementado,['ObterURLAmbiente',ClassName])));
end;

procedure TACBrPSP.VerificarAutenticacao;
begin
  if not Autenticado then
    Autenticar;

  VerificarValidadeToken;
end;

{ TACBrPixRecebedor }

constructor TACBrPixRecebedor.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrPixRecebedor.Clear;
begin
  fNome := '';
  fCidade := '';
  fCEP := '';
  fCodCategoriaComerciante := 0;
end;

procedure TACBrPixRecebedor.Assign(Source: TACBrPixRecebedor);
begin
  fNome := Source.Nome;
  fCidade := Source.Cidade;
  fCEP := Source.CEP;
  fCodCategoriaComerciante := Source.CodCategoriaComerciante;
end;

procedure TACBrPixRecebedor.SetCEP(AValue: String);
begin
  if (fCEP = AValue) then
    Exit;

  fCEP := OnlyNumber(AValue);
end;

procedure TACBrPixRecebedor.SetCidade(AValue: String);
begin
  if (fCidade = AValue) then
    Exit;

  fCidade := copy(Trim(AValue),1,15);
end;

procedure TACBrPixRecebedor.SetNome(AValue: String);
begin
  if (fNome = AValue) then
    Exit;

  fNome := copy(Trim(AValue),1,25);
end;

procedure TACBrPixRecebedor.SetCodCategoriaComerciante(AValue: Integer);
begin
  if (fCodCategoriaComerciante = AValue) then
    Exit;

  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(ACBrStr(sErroMCCForaDaFaixa));

  fCodCategoriaComerciante := AValue;
end;

{ TACBrHttpProxy }

constructor TACBrHttpProxy.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrHttpProxy.Clear;
begin
  fHost := '';
  fPass := '';
  fPort := '';
  fUser := '';
end;

procedure TACBrHttpProxy.Assign(Source: TACBrHttpProxy);
begin
  fHost := Source.Host;
  fPass := Source.Pass;
  fPort := Source.Port;
  fUser := Source.User;
end;

{ TACBrPixCD }

constructor TACBrPixCD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRecebedor := TACBrPixRecebedor.Create;
  fProxy := TACBrHttpProxy.Create;
  fTimeOut := ChttpTimeOutDef;
  fArqLOG := '';
  fNivelLog := 1;
  fAmbiente := ambTeste;
  fQuandoGravarLog := Nil;
end;

destructor TACBrPixCD.Destroy;
begin
  fRecebedor.Free;
  fProxy.Free;

  inherited Destroy;
end;

procedure TACBrPixCD.RegistrarLog(const ALinha: String; NivelLog: Byte);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned(fQuandoGravarLog) then
    fQuandoGravarLog(ALinha, Tratado);

  if not Tratado then
    GravarLogEmArquivo(ALinha);
end;

procedure TACBrPixCD.DispararExcecao(E: Exception);
begin
  if not Assigned(E) then
    Exit;

  RegistrarLog(E.ClassName + ': ' + E.Message);
  raise E;
end;

procedure TACBrPixCD.GravarLogEmArquivo(const ALinha: String);
begin
  if (fArqLOG = '') then
    Exit;

  WriteLog( fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + ALinha);
end;

procedure TACBrPixCD.SetACBrPSP(AValue: TACBrPSP);
var
  va: TACBrPSP ;
begin
  if (AValue = fPSP) then
    Exit;

  if (AValue <> Nil) then
    RegistrarLog('Atribuindo PSP: '+AValue.ClassName+', Nome: '+AValue.Name)
  else
    RegistrarLog('Atribuindo PSP: Nil');

  if Assigned(fPSP) then
    fPSP.RemoveFreeNotification(Self);

  va := fPSP;       // Usa outra variavel para evitar Loop Infinito,
  fPSP := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.ACBrPixCD) then
      va.ACBrPixCD := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.ACBrPixCD := Self;
  end ;
end;

procedure TACBrPixCD.SetProxy(AValue: TACBrHttpProxy);
begin
  if (fProxy <> AValue) then
    fProxy.Assign(AValue);
end;

procedure TACBrPixCD.SetRecebedor(AValue: TACBrPixRecebedor);
begin
  if (fRecebedor <> AValue) then
    fRecebedor.Assign(AValue);
end;

procedure TACBrPixCD.VerificarPSPAtribuido;
begin
  if not Assigned(fPSP) then
    DispararExcecao(EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido)));
end;

procedure TACBrPixCD.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (fPSP <> nil) and (AComponent = fPSP) then
  begin
    RegistrarLog('Removendo PSP: '+fPSP.ClassName+', Nome: '+fPSP.Name);
    fPSP := nil ;
  end;
end;

function TACBrPixCD.GerarQRCodeEstatico(Valor: Currency; infoAdicional: String;
  TxId: String): String;
var
  Erros: String;
  QRCodeEstatico: TACBrPIXQRCodeEstatico;
begin
  VerificarPSPAtribuido;
  RegistrarLog('GerarQRCodeEstatico( '+FloatToString(Valor)+', '+
                                       infoAdicional+', '+
                                       TxId+' )');

  Erros := '';
  if (fRecebedor.Nome = '') then
    Erros := Erros + sErroRecebedorNome + sLineBreak;

  if (fRecebedor.Cidade = '') then
    Erros := Erros + sErroRecebedorCidade + sLineBreak;

  if (fPSP.ChavePIX = '') then
    Erros := Erros + sErroPSPChavePIX + sLineBreak;

  if (fPSP.TipoChave = tchNenhuma) then
    Erros := Erros + sErroPSPTipoChave + sLineBreak;

  if (Erros <> '') then
    DispararExcecao(EACBrPixException.Create(ACBrStr(Erros)));

  QRCodeEstatico := TACBrPIXQRCodeEstatico.Create;
  try
    QRCodeEstatico.NomeRecebedor := fRecebedor.Nome;
    QRCodeEstatico.CidadeRecebedor := fRecebedor.Cidade;
    QRCodeEstatico.CEPRecebedor := fRecebedor.CEP;
    QRCodeEstatico.ChavePix := fPSP.ChavePIX;
    QRCodeEstatico.Valor := Valor;
    QRCodeEstatico.infoAdicional := infoAdicional;
    QRCodeEstatico.TxId := TxId;

    Result := QRCodeEstatico.QRCode;
    RegistrarLog('   '+Result);
  finally
    QRCodeEstatico.Free;
  end;
end;

end.

