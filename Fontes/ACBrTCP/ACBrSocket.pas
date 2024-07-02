{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrSocket;

interface

{$IFDEF VCL}
  {$DEFINE UPDATE_SCREEN_CURSOR}
{$ENDIF}

{$IFDEF LCL}
  {$DEFINE UPDATE_SCREEN_CURSOR}
{$ENDIF}

{$IFDEF NOGUI}
  {$UNDEF UPDATE_SCREEN_CURSOR}
{$ENDIF}

uses
  SysUtils, Classes, Types, syncobjs, ACBrBase,
  { Units da Synapse: }
  blcksock, synsock, httpsend, ssl_openssl
  { Units para a auto-detecção de Proxy: }
  {$IFDEF MSWINDOWS}
    ,windows, wininet
  {$ENDIF};

const 
  cHTTPTimeOutDef = 90000;
  cHTTPMethodGET = 'GET';
  cHTTPMethodPOST = 'POST';
  cHTTPMethodPUT = 'PUT';
  cHTTPMethodPATCH = 'PATCH';
  cHTTPMethodDELETE = 'DELETE';

  cContentTypeUTF8 = 'charset=utf-8';
  cContentTypeTextPlain = 'text/plain';
  cContentTypeApplicationJSon = 'application/json';
  cContentTypeApplicationWwwFormUrlEncoded = 'application/x-www-form-urlencoded';

  cHTTPHeaderAuthorization = 'Authorization:';
  cHTTPHeaderContentType = 'Content-Type:';
  cHTTPHeaderContentEncoding = 'Content-Encoding:';
  cHTTPHeaderAcceptEncoding = 'Accept-Encoding:';

  cHTTPAuthorizationBearer = 'Bearer';
  cHTTPAuthorizationBasic = 'Basic';
  cHTTPProtocols: array[0..2] of string = ('http','https', 'ftp');

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

type

  TACBrTCPServer = class;
  EACBrHTTPError = class(Exception);

  { Evento disparada quando Conecta }
  TACBrTCPServerConecta = procedure( const TCPBlockSocket : TTCPBlockSocket;
    var Enviar : AnsiString ) of object ;

  { Evento disparada quando DesConecta }
  TACBrTCPServerDesConecta = procedure( const TCPBlockSocket : TTCPBlockSocket;
    Erro: Integer; ErroDesc : String ) of object ;

  { Evento disparado quando recebe dados }
  TACBrTCPServerRecive = procedure( const TCPBlockSocket : TTCPBlockSocket;
    const Recebido : AnsiString; var Enviar : AnsiString) of object;

  TACBrOnAntesAbrirHTTP = procedure(var AURL: String) of object;

  THttpContentEncodingCompress = (ecGzip, ecCompress, ecDeflate);
  THttpContentsEncodingCompress = set of THttpContentEncodingCompress;

  { TACBrTCPServerDaemon }

  TACBrTCPServerDaemon = class(TThread)
  private
    fsEnabled: Boolean;
    fsEvent: TSimpleEvent;
    fsSock: TTCPBlockSocket;
    fsACBrTCPServer: TACBrTCPServer ;
    procedure SetEnabled(AValue: Boolean);

  protected
    property ACBrTCPServer : TACBrTCPServer read fsACBrTCPServer ;

  public
    Constructor Create( const AACBrTCPServer : TACBrTCPServer );
    Destructor Destroy; override;
    procedure Execute; override;

    property TCPBlockSocket: TTCPBlockSocket read fsSock ;
    property Enabled: Boolean read fsEnabled write SetEnabled ;
  end;

  { TACBrTCPServerThread }

  TACBrTCPServerThread = class(TThread)
  private
    fsACBrTCPServerDaemon : TACBrTCPServerDaemon ;
    fsEnabled: Boolean;
    fsEvent: TSimpleEvent;
    fsSock: TTCPBlockSocket;
    fsStrRcv, fsStrToSend: AnsiString ;
    fsClientSocket: TSocket;
    fsErro: Integer ;

    function GetActive: Boolean;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure CallOnRecebeDados ;
    procedure CallOnConecta ;
    procedure CallOnDesConecta ;
  public
    Constructor Create(ClientSocket: TSocket; ACBrTCPServerDaemon : TACBrTCPServerDaemon);
    Destructor Destroy; override;

    procedure Execute; override;

    property Enabled: Boolean read fsEnabled write SetEnabled;
    property Active : Boolean read GetActive ;
    property TCPBlockSocket : TTCPBlockSocket read fsSock ;
  end;

  { Componente ACBrTCPServer - Servidor TCP muito simples }

  { TACBrTCPServer }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrTCPServer = class( TACBrComponent )
  private
    { Propriedades do Componente ACBrTCPServer }
    fsACBrTCPServerDaemon: TACBrTCPServerDaemon ;
    fsTimeOut: Integer;
    fsIP: String;
    fsPort: String;
    fsThreadList: TThreadList ;
    fsOnConecta: TACBrTCPServerConecta;
    fsOnRecebeDados: TACBrTCPServerRecive;
    fsOnDesConecta: TACBrTCPServerDesConecta;
    fsTerminador: String;
    fsUsaSynchronize: Boolean;
    fsWaitsInterval: Integer;
    fs_Terminador: AnsiString;
    function GetTCPBlockSocket: TTCPBlockSocket ;
    procedure SetAtivo(const Value: Boolean);
    procedure SetIP(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetTerminador( const AValue: String) ;
    procedure SetTimeOut(const Value: Integer);
    procedure SetUsaSynchronize(AValue: Boolean);
    procedure SetWaitInterval(AValue: Integer);

    procedure VerificaAtivo ;
    function GetAtivo: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override;

    procedure Ativar ;
    procedure Desativar ;
    procedure EnviarString(const AString : AnsiString; NumConexao : Integer = -1 ) ;
    procedure Terminar(NumConexao : Integer = -1 ) ;

    property Ativo : Boolean read GetAtivo write SetAtivo ;
    property ThreadList : TThreadList read fsThreadList ;
    property StrTerminador : AnsiString  read fs_Terminador  ;

     { Instancia do Componente TACBrTCPServerDaemon }
    property ACBrTCPServerDaemon : TACBrTCPServerDaemon read fsACBrTCPServerDaemon ;
    property TCPBlockSocket : TTCPBlockSocket read GetTCPBlockSocket ;
  published
    property IP         : String  read fsIP         write SetIP;
    property Port       : String  read fsPort       write SetPort ;
    property TimeOut    : Integer read fsTimeOut    write SetTimeOut
      default 5000;
    property WaitInterval: Integer read fsWaitsInterval write SetWaitInterval default 200;
    property Terminador    : String  read fsTerminador     write SetTerminador;
    property UsaSynchronize: Boolean read fsUsaSynchronize write SetUsaSynchronize
      default {$IFNDEF NOGUI}True{$ELSE}False{$ENDIF};

    property OnConecta     : TACBrTCPServerConecta    read  fsOnConecta
                                                      write fsOnConecta ;
    property OnDesConecta  : TACBrTCPServerDesConecta read  fsOnDesConecta
                                                      write fsOnDesConecta ;
    property OnRecebeDados : TACBrTCPServerRecive     read  fsOnRecebeDados
                                                      write fsOnRecebeDados ;
end;

  { TACBrHTTPQueryParams }

  TACBrHTTPQueryParams = class(TStringList)
  private
    function GetAsURL: String;
    procedure SetAsURL(const aValue: String);
  public
    property AsURL: String read GetAsURL write SetAsURL;
  end;

  { TACBrHTTP }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrHTTP = class(TACBrComponent)
  private
    fArqLOG: String;
    fHTTPResultCode: Integer;
    fNivelLog: Byte;
    FIsUTF8: Boolean;
    FTimeOut: Integer;
    fHTTPSend: THTTPSend;
    fHTTPResponse: AnsiString;
    fURLPathParams: TStringList;
    fHttpRespStream: TMemoryStream;
    fOnQuandoGravarLog: TACBrGravarLog;
    fURLQueryParams: TACBrHTTPQueryParams;
    fOnAntesAbrirHTTP: TACBrOnAntesAbrirHTTP;
    fContenstEncodingCompress: THttpContentsEncodingCompress;
    fURL: String;
    function GetProxyHost: String;
    function GetProxyPass: String;
    function GetProxyPort: String;
    function GetProxyUser: String;
    function GetRespIsUTF8: Boolean;
    procedure SetProxyHost(const AValue: String);
    procedure SetProxyPass(const AValue: String);
    procedure SetProxyPort(const AValue: String);
    procedure SetProxyUser(const AValue: String);
  protected
    function GetHeaderValue(aHeader: String): String;
    procedure RegistrarLog(const aLinha: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AjustaParam(const AParam: String): String;

    procedure HTTPGet(const AURL: String); virtual;
    procedure HTTPPost(const AURL: String); overload; virtual;
    procedure HTTPPost(const AURL: String; const APostData: AnsiString); overload; virtual;
    procedure HTTPPut(const AURL: String);
    procedure HTTPMethod(const Method, AURL: String); virtual;

    procedure LimparHTTP;
    procedure LerConfiguracoesProxy; 

    property HTTPSend: THTTPSend read fHTTPSend;
    property HTTPResponse: AnsiString read fHTTPResponse;
    property HTTPResultCode: Integer read fHTTPResultCode;
    property URL: String read fURL;

    property URLPathParams: TStringList read fURLPathParams;
    property URLQueryParams: TACBrHTTPQueryParams read fURLQueryParams;
  published
    property ProxyHost: String read GetProxyHost write SetProxyHost;
    property ProxyPort: String read GetProxyPort write SetProxyPort;
    property ProxyUser: String read GetProxyUser write SetProxyUser;
    property ProxyPass: String read GetProxyPass write SetProxyPass;
    property RespIsUTF8: Boolean read GetRespIsUTF8;
    property TimeOut: Integer read FTimeOut write FTimeOut default 90000;
    property ContentsEncodingCompress: THttpContentsEncodingCompress read fContenstEncodingCompress write fContenstEncodingCompress;
                
    property ArqLOG: String read fArqLOG write fArqLOG;
    property NivelLog: Byte read fNivelLog write fNivelLog default 1;
    property OnQuandoGravarLog: TACBrGravarLog read fOnQuandoGravarLog write fOnQuandoGravarLog;
    property OnAntesAbrirHTTP: TACBrOnAntesAbrirHTTP read fOnAntesAbrirHTTP write fOnAntesAbrirHTTP;
end;

function GetURLBasePath(const URL: String): String;
function IsAbsoluteURL(const URL: String): Boolean;
function URLWithDelim(aURL: String): String;
function URLWithoutDelim(aURL: String): String;
function GetHeaderValue(const aValue: String; aStringList: TStringList): String;
function ContentIsCompressed(aHeader: TStringList): Boolean;
function DecompressStream(aStream: TStream): AnsiString;
function ContentEncodingCompressToString(aValue: THttpContentEncodingCompress): String;
function StringToContentEncodingCompress(aValue: String): THttpContentEncodingCompress;

implementation

uses
  math, StrUtils, synacode,
  ACBrCompress,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  synautil
  {$IFDEF UPDATE_SCREEN_CURSOR}
    ,Controls, Forms
  {$ENDIF};

function GetURLBasePath(const URL: String): String;
begin
  Result := Copy(URL, 1, PosLast('/',URL) );
end;

function IsAbsoluteURL(const URL: String): Boolean;
var
 i: Integer;
begin
  Result := False;

  //Testa se é um tipo absoluto relativo ao protocolo
  if Pos('//', URL) = 1 then
  begin
    Result := True;
    Exit;
  end;

  //Testa se é um tipo relativo
  if Pos('/', URL) = 1 then
  begin
    Result := False;
    Exit;
  end;

  //Testa se inicia por protocolos...
  for I := 0 to High(cHTTPProtocols) do
  begin
    if Pos(UpperCase(cHTTPProtocols[i])+'://', UpperCase(URL)) = 1 then
    begin
      Result := True;
      Break;
    end;
  end;

  if Result then Exit;

  //Começa com "www."
  if Pos('www.', URL) = 1 then
  begin
    Result := True;
    Exit;
  end;
end;

function URLWithDelim(aURL: String): String;
begin
  Result := Trim(aURL);
  if (RightStr(Result, 1) <> '/') then
    Result := Result + '/'
end;

function URLWithoutDelim(aURL: String): String;
begin
  Result := Trim(aURL);
  while NaoEstaVazio(Result) and (RightStr(Result, 1) = '/') do
    Delete(Result, Length(Result), 1);
end;

function GetHeaderValue(const aValue: String; aStringList: TStringList): String;
var
  i: Integer;
  u, LinhaHeader: String;
begin
  Result := '';
  u := UpperCase(Trim(AValue));
  if EstaVazio(u) then
    Exit;

  i := 0;
  while EstaVazio(Result) and (i < aStringList.Count) do
  begin
    LinhaHeader := aStringList[i];
    if (Pos(u, UpperCase(LinhaHeader)) = 1) then
      Result := Trim(Copy(LinhaHeader, Length(u)+1, Length(LinhaHeader)));
    Inc(i);
  end;
end;

function ContentIsCompressed(aHeader: TStringList): Boolean;
var
  i: Integer;
  ce: String;
begin
  Result := False;
  ce := GetHeaderValue(cHTTPHeaderContentEncoding, aHeader);

  for i := Ord(Low(THttpContentEncodingCompress)) to Ord(High(THTTPContentEncodingCompress)) do
  begin
    Result := (Pos(ContentEncodingCompressToString(THttpContentEncodingCompress(i)), ce) > 0);
    if Result then
      Exit;
  end;
end;

function DecompressStream(aStream: TStream): AnsiString;
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

function ContentEncodingCompressToString(aValue: THttpContentEncodingCompress): String;
begin
  Result := EmptyStr;
  case aValue of
    ecGzip: Result := 'gzip';
    ecCompress: Result := 'compress';
    ecDeflate: Result := 'deflate';
  end;
end;

function StringToContentEncodingCompress(aValue: String): THttpContentEncodingCompress;
begin
  aValue := LowerCase(aValue);
  if (aValue = 'gzip') then
    Result := ecGzip
  else if (aValue = 'compress') then
    Result := ecCompress
  else if (aValue = 'deflate') then
    Result := ecDeflate;
end;

{ TACBrHTTPQueryParams }

function TACBrHTTPQueryParams.GetAsURL: String;
var
  i: Integer;
  aName, aValue: String;
begin
  Result := EmptyStr;
  if EstaZerado(Count) then
    Exit;

  for i := 0 to Count-1 do
  begin
    aName := Names[i];
    if NaoEstaVazio(aName) then
    begin
      if NaoEstaVazio(Result) then
        Result := Result + '&';
      aValue := Values[aName];
      Result := Result + EncodeURLElement(aName) + '=' + EncodeURLElement(aValue);
    end;
  end;
end;

procedure TACBrHTTPQueryParams.SetAsURL(const aValue: String);
var
  s: String;
begin
  Clear;
  s := Trim(aValue);
  if (Copy(s, 1, 1) = '?') then
    System.Delete(s, 1, 1);

  s := DecodeURL(s);
  AddDelimitedTextToList(s, '&', Self, #0);
end;

{ TACBrTCPServerDaemon }

constructor TACBrTCPServerDaemon.Create( const AACBrTCPServer : TACBrTCPServer );
begin
  fsACBrTCPServer := AACBrTCPServer ;
  fsEvent         := TSimpleEvent.Create;
  fsSock          := TTCPBlockSocket.create ;
  fsEnabled       := False;

  inherited Create(False);
  FreeOnTerminate := False;
end;

destructor TACBrTCPServerDaemon.Destroy;
begin
  fsSock.CloseSocket;
  fsEnabled := False;
  Terminate;
  fsEvent.SetEvent;  // libera Event.WaitFor()
  WaitFor;

  fsEvent.Free;
  fsSock.Free ;

  inherited Destroy;
end;

procedure TACBrTCPServerDaemon.SetEnabled(AValue: Boolean);
begin
  if fsEnabled = AValue then Exit;

  fsEnabled := AValue;
  if not AValue then
    fsSock.CloseSocket;

  fsEvent.SetEvent;
end;

procedure TACBrTCPServerDaemon.Execute;
var
  ClientSock : TSocket;
begin
  while (not Terminated) and Assigned(fsSock) do
  begin
    fsEvent.ResetEvent;

    if fsEnabled then
      fsEnabled := (fsSock.Socket <> INVALID_SOCKET);   // O Socket ainda é válido ?

    if fsEnabled then
    begin
      with fsSock do
      begin
        if CanRead( fsACBrTCPServer.WaitInterval ) then
        begin
          if fsEnabled and (LastError = 0) then
          begin
            ClientSock := Accept;
            TACBrTCPServerThread.Create(ClientSock, Self);
          end;
        end;
      end ;
    end
    else
      fsEvent.WaitFor( Cardinal(-1) );  // Espera até a chamada de ResetEvent();
  end;

  Terminate;
end;

{ TACBrTCPServerThread }

constructor TACBrTCPServerThread.Create(ClientSocket: TSocket;
  ACBrTCPServerDaemon: TACBrTCPServerDaemon);
begin
  fsEnabled := True;
  fsEvent := TSimpleEvent.Create;
  fsACBrTCPServerDaemon := ACBrTCPServerDaemon ;
  fsClientSocket := ClientSocket ;
  FreeOnTerminate := True ;

  inherited create(false);
end;

destructor TACBrTCPServerThread.Destroy;
begin
  fsEnabled := False;
  Terminate;
  fsEvent.SetEvent;  // libera Event.WaitFor()

  if not Terminated then
    WaitFor;

  fsEvent.Free;
  inherited Destroy;
end;

procedure TACBrTCPServerThread.SetEnabled(AValue: Boolean);
begin
  if fsEnabled = AValue then Exit;

  fsEnabled := AValue;
  fsEvent.SetEvent;
end;

procedure TACBrTCPServerThread.Execute;
begin
  fsStrToSend := '' ;
  fsErro      := 0 ;
  fsSock      := TTCPBlockSocket.Create;
  try
    fsSock.Socket := fsClientSocket ;
    fsSock.GetSins;
    with fsSock do
    begin
      fsSock.Owner := Self;

      if fsACBrTCPServerDaemon.ACBrTCPServer.UsaSynchronize then
        Synchronize(CallOnConecta)
      else
        CallOnConecta;

      if (fsStrToSend <> '') then
      begin
        SendString( fsStrToSend );
        fsErro := LastError ;
      end ;

      while (fsErro = 0) do
      begin
        fsEvent.ResetEvent;

        if Terminated then
        begin
          fsErro := -1 ;
          break;
        end ;

        if fsSock.Socket = INVALID_SOCKET then   // O Socket ainda é válido ?
        begin
          fsErro := -2 ;
          break;
        end ;

        if not Assigned( fsACBrTCPServerDaemon ) then  // O Daemon ainda existe ?
        begin
          fsErro := -5 ;
          break ;
        end ;

        if not fsACBrTCPServerDaemon.Enabled then  // O Daemon não está ativo ?
        begin
          fsErro := -3 ;
          break ;
        end ;

        if fsACBrTCPServerDaemon.Terminated then   // O Daemon está rodando ?
        begin
          fsErro := -4 ;
          break ;
        end ;

        if not fsEnabled then
        begin
          fsEvent.WaitFor( Cardinal(-1) );   // Espera infinita, até chamada de SetEvent();
          Continue;
        end;

        // Se não tem nada para ler, re-inicia o loop //
        if not fsSock.CanRead( fsACBrTCPServerDaemon.ACBrTCPServer.WaitInterval ) then
          Continue ;

        if not Terminated then
        begin
          // Se tem Terminador, lê até chagar o Terminador //
          if fsACBrTCPServerDaemon.ACBrTCPServer.StrTerminador <> '' then
            fsStrRcv := RecvTerminated( fsACBrTCPServerDaemon.ACBrTCPServer.TimeOut,
                                        fsACBrTCPServerDaemon.ACBrTCPServer.StrTerminador )
          else
            fsStrRcv := RecvPacket( fsACBrTCPServerDaemon.ACBrTCPServer.TimeOut ) ;

          fsErro := LastError ;
          if fsErro <> 0 then
            break;

          if Assigned( fsACBrTCPServerDaemon.ACBrTCPServer.OnRecebeDados ) then
          begin
            if fsACBrTCPServerDaemon.ACBrTCPServer.UsaSynchronize then
              Synchronize(CallOnRecebeDados)
            else
              CallOnRecebeDados;
          end;
        end;

        if not Terminated then
        begin
          if fsStrToSend <> '' then
          begin
            SendString( fsStrToSend );
            fsErro := LastError ;
          end ;
        end;
      end;
    end;

    Terminate;
  finally
    // Chama o evento de Desconexão...
    if Assigned(fsACBrTCPServerDaemon) then
    begin
      if not fsACBrTCPServerDaemon.Terminated then
      begin
        if fsACBrTCPServerDaemon.ACBrTCPServer.UsaSynchronize then
          Synchronize(CallOnDesConecta)
        else
          CallOnDesConecta;
      end;
    end;

    fsSock.CloseSocket ;
    FreeAndNil(fsSock);
  end;
end;

procedure TACBrTCPServerThread.CallOnRecebeDados;
begin
  if not Assigned(fsACBrTCPServerDaemon) then exit;

  fsStrToSend := '' ;
  fsACBrTCPServerDaemon.ACBrTCPServer.OnRecebeDados( fsSock, fsStrRcv, fsStrToSend ) ;
end;

procedure TACBrTCPServerThread.CallOnConecta;
begin
  if not Assigned(fsACBrTCPServerDaemon) then exit;

  // Adiciona essa Thread na Lista de Threads
  fsACBrTCPServerDaemon.ACBrTCPServer.ThreadList.Add( Self ) ;

  // Chama o Evento, se estiver atribuido
  if Assigned( fsACBrTCPServerDaemon.ACBrTCPServer.OnConecta ) then
  begin
    fsStrToSend := '' ;
    fsACBrTCPServerDaemon.ACBrTCPServer.OnConecta( fsSock, fsStrToSend ) ;
  end ;
end;

procedure TACBrTCPServerThread.CallOnDesConecta;
 Var ErroDesc : String ;
begin
  if not Assigned(fsACBrTCPServerDaemon) then exit;

  // Remove essa Thread da Lista de Threads. Se estiver matando o Daemon, ele limpa a lista...
  if not fsACBrTCPServerDaemon.Terminated then
    fsACBrTCPServerDaemon.ACBrTCPServer.ThreadList.Remove( Self ) ;

  // Chama o Evento, se estiver atribuido
  if Assigned( fsACBrTCPServerDaemon.ACBrTCPServer.OnDesConecta ) then
  begin
    ErroDesc := fsSock.GetErrorDesc( fsErro ) ;
    fsACBrTCPServerDaemon.ACBrTCPServer.OnDesConecta( fsSock, fsErro, ErroDesc ) ;
  end ;
end;

function TACBrTCPServerThread.GetActive: Boolean;
begin
  Result := (not self.Terminated) and (fsSock.LastError = 0) ;
end;


{ TACBrTCPServer }

constructor TACBrTCPServer.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fsIP             := '0.0.0.0';
  fsPort           := '0';
  fsTimeOut        := 5000;
  fsTerminador     := '';
  fs_Terminador    := '';
  fsWaitsInterval  := 200;
  fsUsaSynchronize := {$IFNDEF NOGUI}True{$ELSE}False{$ENDIF};

  fsACBrTCPServerDaemon := TACBrTCPServerDaemon.Create( Self );
  fsThreadList := TThreadList.Create;
end;

destructor TACBrTCPServer.Destroy;
begin
  fsOnDesConecta := Nil;
  Desativar;

  fsACBrTCPServerDaemon.Free;
  fsThreadList.Free ;

  inherited Destroy;
end;

function TACBrTCPServer.GetAtivo: Boolean;
begin
  if Assigned( fsACBrTCPServerDaemon ) then
    Result := fsACBrTCPServerDaemon.Enabled
  else
    Result := False;
end;

procedure TACBrTCPServer.SetAtivo(const Value: Boolean);
begin
  if Value then
    Ativar
  else
    Desativar ;
end;

function TACBrTCPServer.GetTCPBlockSocket: TTCPBlockSocket ;
begin
  Result := nil ;
  if Assigned( fsACBrTCPServerDaemon ) then
    Result := fsACBrTCPServerDaemon.TCPBlockSocket ;
end;

procedure TACBrTCPServer.Ativar;
Var
  Erro : Integer ;
  ErroDesc : String ;
begin
  if Ativo then Exit;

  with fsACBrTCPServerDaemon.TCPBlockSocket do
  begin
    CloseSocket;
    SetLinger(True,10);
    Bind( fsIP , fsPort );
    if LastError = 0 then
      Listen;

    Erro     := LastError;
    ErroDesc := LastErrorDesc;
  end;

  if Erro = 0 then
    fsACBrTCPServerDaemon.Enabled := True  // Inicia o Loop de Escuta
  else
  begin
    Desativar;
    raise Exception.Create( 'Erro: '+IntToStr(Erro)+' - '+ErroDesc+sLineBreak+
                            ACBrStr('Não foi possível criar serviço na porta: ')+Port ) ;
  end ;
end;

procedure TACBrTCPServer.Desativar;
var
  I: Integer;
  UmaConexao: TACBrTCPServerThread;
begin
  if not Ativo then Exit;

  fsACBrTCPServerDaemon.Enabled := False;

  with fsThreadList.LockList do
  try
    I := Count-1;
    while I >= 0 do
    begin
      UmaConexao := TACBrTCPServerThread(Items[I]);
        
      // Chama o Evento de Desconexão manualmente...
      if Assigned( fsOnDesConecta ) then
        fsOnDesConecta( UmaConexao.TCPBlockSocket, -5, 'TACBrTCPServer.Desativar' ) ;

      UmaConexao.Terminate;
      Dec( I );
    end
  finally
    fsThreadList.UnlockList;
  end ;

  fsThreadList.Clear ;

  // Chama o Evento mais uma vez, porém sem nenhuma conexão,
  if Assigned( fsOnDesConecta ) then
    fsOnDesConecta( Nil, -6, 'TACBrTCPServer.Desativar' ) ;
end;

procedure TACBrTCPServer.SetIP(const Value: String);
begin
  VerificaAtivo ;
  fsIP := Value;
end;

procedure TACBrTCPServer.SetPort(const Value: String);
begin
  if fsPort = Value then exit;

  VerificaAtivo ;
  fsPort := Value;
end;

procedure TACBrTCPServer.SetTerminador( const AValue: String) ;
begin
  if fsTerminador = AValue then exit;

  VerificaAtivo ;
  fsTerminador  := AValue;
  fs_Terminador := TraduzComando( fsTerminador ) ;

  if (fs_Terminador = '') and (AValue <> '') then  // não usou notação '#13,#10'
  begin
    fs_Terminador := AnsiString(AValue);
    fsTerminador := StringToAsc(fs_Terminador);
  end;
end;

procedure TACBrTCPServer.SetTimeOut(const Value: Integer);
begin
  fsTimeOut := Value;
end;

procedure TACBrTCPServer.SetUsaSynchronize(AValue: Boolean);
begin
  if (fsUsaSynchronize = AValue) then
    Exit;

  {$IFNDEF NOGUI}
    fsUsaSynchronize := AValue;
  {$ELSE}
    fsUsaSynchronize := False;
  {$ENDIF};
end;

procedure TACBrTCPServer.SetWaitInterval(AValue: Integer);
begin
  if fsWaitsInterval = AValue then Exit;
  fsWaitsInterval := min( max(AValue,10), 5000);
end;

procedure TACBrTCPServer.VerificaAtivo;
begin
  if Ativo then
     raise Exception.Create( ACBrStr('Não é possível modificar as propriedades com '+
                             'o componente Ativo') );
end;

procedure TACBrTCPServer.EnviarString(const AString : AnsiString;
   NumConexao : Integer = -1 ) ;
Var
  I : Integer ;
begin
  if not Ativo then
     raise Exception.Create(ACBrStr('Componente ACBrTCPServer não está ATIVO'));

  with fsThreadList.LockList do
  try
     if NumConexao < 0 then
      begin
        For I := 0 to Count-1 do
           TACBrTCPServerThread(Items[I]).TCPBlockSocket.SendString( AString );
      end
     else
      begin
        if (NumConexao >= Count) then
           raise Exception.Create(ACBrStr('Numero de conexão inexistente: ')+IntToStr(NumConexao));

        TACBrTCPServerThread(Items[NumConexao]).TCPBlockSocket.SendString( AString );
      end ;
  finally
     fsThreadList.UnlockList;
  end ;
end;

procedure TACBrTCPServer.Terminar(NumConexao : Integer) ;
Var
  I : Integer ;
begin
  with fsThreadList.LockList do
  try
     if NumConexao < 0 then
      begin
        For I := 0 to Count-1 do
        begin
           TACBrTCPServerThread(Items[I]).TCPBlockSocket.CloseSocket ;
           TACBrTCPServerThread(Items[I]).Terminate ;
        end ;
      end
     else
      begin
        if (NumConexao >= Count) then
           raise Exception.Create(ACBrStr('Numero de conexão inexistente: ')+IntToStr(NumConexao));

        TACBrTCPServerThread(Items[NumConexao]).TCPBlockSocket.CloseSocket;
        TACBrTCPServerThread(Items[NumConexao]).Terminate;
      end ;
  finally
     fsThreadList.UnlockList;
  end ;
end ;

{ TACBrHTTP }

constructor TACBrHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fURLQueryParams := TACBrHTTPQueryParams.Create;
  fURLPathParams := TStringList.Create;
  fHttpRespStream := TMemoryStream.Create;
  fHttpSend := THTTPSend.Create;
  fHttpSend.OutputStream := fHttpRespStream;
  fOnAntesAbrirHTTP := Nil;
  fHTTPResultCode := 0;
  fURL := EmptyStr;
  FIsUTF8 := False;
  FTimeOut := cHTTPTimeOutDef;
  fContenstEncodingCompress := [];
end;

destructor TACBrHTTP.Destroy;
begin
  fHTTPSend.Free;
  fHttpRespStream.Free;
  fURLQueryParams.Free;
  fURLPathParams.Free;
  inherited Destroy;
end;

function TACBrHTTP.AjustaParam(const AParam: String): String;
begin
  Result := Trim(AParam);

  if (Result <> '') then
  begin
    Result := String(ACBrStrToAnsi(Result));
    Result := String(EncodeURLElement(AnsiString(Result)));
  end;
end;

procedure TACBrHTTP.HTTPGet(const AURL: String);
begin
  HTTPSend.Clear;
  HTTPMethod(cHTTPMethodGET, AURL);
end ;

procedure TACBrHTTP.HTTPPost(const AURL: String);
begin
  HTTPMethod(cHTTPMethodPOST, AURL);
end;

procedure TACBrHTTP.HTTPPut(const AURL: String);
begin
  HTTPMethod(cHTTPMethodPUT, AURL);
end;

procedure TACBrHTTP.HTTPPost(const AURL: String; const APostData: AnsiString);
begin
  HTTPSend.Clear;
  HTTPSend.Document.Write(Pointer(APostData)^, Length(APostData));
  RegistrarLog('Req.Body: ' + APostData);
  HTTPPost(AURL);
end;

procedure TACBrHTTP.HTTPMethod(const Method, AURL: String);
var
  {$IFDEF UPDATE_SCREEN_CURSOR}
   OldCursor: TCursor;
  {$ENDIF}
   HeadersOld, qp, Location, wErro: String;
   //wCEC: String;
   i, ContaRedirecionamentos: Integer;
   ce: THttpContentEncodingCompress;
   AddUTF8InHeader: Boolean;
begin
  {$IFDEF UPDATE_SCREEN_CURSOR}
   OldCursor := Screen.Cursor;
   Screen.Cursor := crHourGlass;
  {$ENDIF}
  ContaRedirecionamentos := 0;
  try
    fHTTPResponse := EmptyStr;
    fURL := AURL;

    {$IfDef UNICODE}
     AddUTF8InHeader := True;
    {$Else}
     AddUTF8InHeader := FIsUTF8;
    {$EndIf}

    {wCEC := EmptyStr;
    for ce in fContenstEncodingCompress do
      if EstaVazio(wCEC) then
        wCEC := cHTTPHeaderAcceptEncoding + ContentEncodingCompressToString(ce)
      else
        wCEC := wCEC + ',' + ContentEncodingCompressToString(ce);

    if NaoEstaVazio(wCEC) then
      fHttpSend.Headers.Add(wCEC);}

    if NaoEstaZerado(fURLPathParams.Count) then
      for i := 0 to fURLPathParams.Count - 1 do
        fURL := URLWithDelim(fURL) + URLWithoutDelim(EncodeURLElement(fURLPathParams[i]));

    qp := fURLQueryParams.AsURL;
    if NaoEstaVazio(qp) then
      fURL := fURL + '?' + qp;

    if AddUTF8InHeader then
      HTTPSend.Headers.Add('Accept-Charset: utf-8;q=*;q=0.7');

    if Assigned(OnAntesAbrirHTTP) then
      OnAntesAbrirHTTP(fURL);

    // DEBUG //
    //HTTPSend.Document.SaveToFile( '_HttpSend.txt' );
    RegistrarLog('HTTPMethod( ' + Method + ', URL: ' + fURL + ' )');
    RegistrarLog(' - Req.Headers: ' + HTTPSend.Headers.Text);

    if (FTimeOut > 0) then
    begin
      HTTPSend.Timeout := FTimeOut;
      with HTTPSend.Sock do
      begin
        ConnectionTimeout := FTimeOut;
        InterPacketTimeout := False;
        NonblockSendTimeout := FTimeOut;
        SocksTimeout := FTimeOut;
        HTTPTunnelTimeout := FTimeOut;
      end;
    end;

    if (NivelLog > 3) then
      RegistrarLog(sLineBreak +
        'Http.Sock.SSL.Certificate: ' + HTTPSend.Sock.SSL.Certificate + sLineBreak +
        'Http.Sock.SSL.PrivateKey: ' + HTTPSend.Sock.SSL.PrivateKey + sLineBreak +
        'Http.Sock.SSL.CertificateFile: ' + HTTPSend.Sock.SSL.CertificateFile + sLineBreak +
        'Http.Sock.SSL.PrivateKeyFile: ' + HTTPSend.Sock.SSL.PrivateKeyFile + sLineBreak);

    HeadersOld := HTTPSend.Headers.Text;
    HTTPSend.HTTPMethod(Method, fURL);

    while (ContaRedirecionamentos <= 10) do
    begin
      Inc(ContaRedirecionamentos);

      case  HTTPSend.ResultCode of
        HTTP_MOVED_PERMANENTLY,  // 301
        HTTP_MOVED_TEMPORARILY,  // 302
        HTTP_SEE_OTHER,          // 303
        HTTP_TEMPORARY_REDIRECT: // 307
        begin
          // DEBUG //
          //HTTPSend.Headers.SaveToFile('c:\temp\HeaderResp.txt');

          Location := GetHeaderValue('Location:');
          Location := Trim(SeparateLeft( Location, ';' ));

          //Location pode ser relativa ou absoluta http://stackoverflow.com/a/25643550/460775
          if IsAbsoluteURL(Location) then
            fURL := Location
          else
            fURL := GetURLBasePath(fURL) + Location;

          HTTPSend.Clear;
          HTTPSend.Headers.Text := HeadersOld;

          // Tipo de método usado não deveria ser trocado...
          // https://tools.ietf.org/html/rfc2616#page-62
          // ... mas talvez seja necessário, pois a maioria dos browsers o fazem
          // http://blogs.msdn.com/b/ieinternals/archive/2011/08/19/understanding-the-impact-of-redirect-response-status-codes-on-http-methods-like-head-get-post-and-delete.aspx
          if (HttpSend.ResultCode = HTTP_SEE_OTHER) or
             (((HttpSend.ResultCode = HTTP_MOVED_PERMANENTLY) or
               (HttpSend.ResultCode = HTTP_MOVED_TEMPORARILY)) and (Method = cHTTPMethodPOST)) then
            HTTPSend.HTTPMethod(cHTTPMethodGET, fURL)
          else
            HTTPSend.HTTPMethod(Method, fURL);
        end;
      else
        Break;
      end;
    end;

    fHTTPResultCode := fHttpSend.ResultCode;
    if (NivelLog > 1) then
      RegistrarLog('  ResultCode: ' + IntToStr(fHTTPResultCode));
    if (NivelLog > 2) then
      RegistrarLog('  ResultString: ' + fHttpSend.ResultString);
    if (NivelLog > 3) then
      RegistrarLog('  Sock.LastError: ' + IntToStr(fHttpSend.Sock.LastError));

    if ContentIsCompressed(fHttpSend.Headers) then
    begin
      if (NivelLog > 2) then
        RegistrarLog('    Decompress Content');

      fHTTPResponse := DecompressStream(fHttpSend.OutputStream)
    end
    else
      fHttpSend.OutputStream.Position := 0;
      fHTTPResponse := ReadStrFromStream(fHttpSend.OutputStream, fHttpSend.OutputStream.Size);

    if (NivelLog > 2) then
      RegistrarLog('Resp.Body: ' + sLineBreak + fHTTPResponse);

    if (not (fHTTPResultCode in [HTTP_OK, HTTP_CREATED, HTTP_ACCEPTED])) then
    begin
      wErro :=
        'Erro HTTP: ' + IntToStr(fHTTPResultCode) +' '+ HTTPSend.ResultString + sLineBreak +
        'Socket Error: ' + IntToStr(HTTPSend.Sock.LastError) +' '+ HTTPSend.Sock.LastErrorDesc + sLineBreak +
        'URL: ' + AURL + sLineBreak + sLineBreak +
        'Resposta HTTP:' + sLineBreak + String(AjustaLinhas(fHTTPResponse, 80, 20));

      RegistrarLog(wErro);
      raise EACBrHTTPError.Create(wErro);
    end;
  finally
    {$IFDEF UPDATE_SCREEN_CURSOR}
    Screen.Cursor := OldCursor;
    {$ENDIF}
  end;
end;

procedure TACBrHTTP.LimparHTTP;
begin
  if (NivelLog > 2) then
    RegistrarLog('LimparHTTP');

  fHTTPSend.Clear;
  fURLPathParams.Clear;
  fURLQueryParams.Clear;  
  fHTTPSend.Headers.Clear;
  fHttpSend.UserName := EmptyStr;
  fHttpSend.Password := EmptyStr;
  fHTTPResponse := EmptyStr;
  FHTTPResultCode := 0;
end;

procedure TACBrHTTP.LerConfiguracoesProxy;
{$IFDEF MSWINDOWS}
var
  Len: DWORD;
  i, j: Integer;

  Server, Port, User, Password: String;

  function GetProxyServer: String;
  var
     ProxyInfo: PInternetProxyInfo;
  begin
     Result := '';
     Len    := 0;
     if not InternetQueryOption(nil, INTERNET_OPTION_PROXY, nil, Len) then
     begin
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
           GetMem(ProxyInfo, Len);
           try
              if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len) then
              begin
                 if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
                    Result := String(ProxyInfo^.lpszProxy);
              end;
           finally
              FreeMem(ProxyInfo);
           end;
        end;
     end;
  end;

  function GetOptionString(Option: DWORD): String;
  begin
     Len := 0;
     Result := EmptyStr;
     if not InternetQueryOption(nil, Option, nil, Len) then
     begin
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
           SetLength(Result, Len);
           if InternetQueryOption(nil, Option, Pointer(Result), Len) then
              Exit;
        end;
     end;
  end;

begin
  Port     := '';
  User     := '';
  Password := '';
  Server   := GetProxyServer;

  if Server <> '' then
  begin
     User     := GetOptionString(INTERNET_OPTION_PROXY_USERNAME);
     Password := GetOptionString(INTERNET_OPTION_PROXY_PASSWORD);

     i := Pos('http=', Server);
     if i > 0 then
     begin
        Delete(Server, 1, i+5);
        j := Pos(';', Server);
        if j > 0 then
           Server := Copy(Server, 1, j-1);
     end;

     i := Pos(':', Server);
     if i > 0 then
     begin
        Port   := Copy(Server, i+1, MaxInt);
        Server := Copy(Server, 1, i-1);
     end;
  end;

  ProxyHost := Server;
  ProxyPort := Port;
  ProxyUser := User;
  ProxyPass := Password;
end;
{$ELSE}
Var
  Arroba, DoisPontos, Barras : Integer ;
  http_proxy, Password, User, Server, Port : String ;
begin
{ http_proxy=http://user:password@proxy:port/
  http_proxy=http://proxy:port/                    }

  http_proxy := Trim(GetEnvironmentVariable( 'http_proxy' )) ;
  if http_proxy = '' then exit ;

  if RightStr(http_proxy,1) = '/' then
     http_proxy := copy( http_proxy, 1, Length(http_proxy)-1 );

  Barras := pos('//', http_proxy);
  if Barras > 0 then
     http_proxy := copy( http_proxy, Barras+2, Length(http_proxy) ) ;

  Arroba     := pos('@', http_proxy) ;
  DoisPontos := pos(':', http_proxy) ;
  Password   := '' ;
  User       := '' ;

  if (Arroba > 0) then
  begin
     if (DoisPontos < Arroba) then
        Password := copy( http_proxy, DoisPontos+1, Arroba-DoisPontos-1 )
     else
        DoisPontos := Arroba;

     User := copy( http_proxy, 1, DoisPontos-1) ;

     http_proxy := copy( http_proxy, Arroba+1, Length(http_proxy) );
  end ;

  DoisPontos := pos(':', http_proxy+':') ;

  Server := copy( http_proxy, 1, DoisPontos-1) ;
  Port   := copy( http_proxy, DoisPontos+1, Length(http_proxy) );

  ProxyHost := Server;
  ProxyPort := Port;
  ProxyUser := User;
  ProxyPass := Password;
end ;
{$ENDIF}

function TACBrHTTP.GetProxyHost: String;
begin
  Result := fHTTPSend.ProxyHost;
end;

function TACBrHTTP.GetRespIsUTF8: Boolean;
var
  wHeaderValue, wHtmlHead: String;
begin
  Result := False;
  if EstaVazio(HTTPResponse) then
    Exit;

  wHeaderValue := LowerCase(GetHeaderValue(cHTTPHeaderContentType));
  Result := (Pos('utf-8', wHeaderValue) > 0);
  if (not Result) and (Pos('xhtml+xml', wHeaderValue) > 0) then
    Result := XmlEhUTF8(HTTPResponse);
  if (not Result) and (pos('html', wHeaderValue) > 0) then
  begin
    wHtmlHead := RetornarConteudoEntre(LowerCase(HTTPResponse),'<head>','</head>');
    Result := (Pos('charset="utf-8"', wHtmlHead) > 0);
  end;
end;

function TACBrHTTP.GetHeaderValue(aHeader: String): String;
var
  wLinhaHeader: String;
  I: Integer;
begin
  Result := EmptyStr;
  for I := 0 to HTTPSend.Headers.Count-1 do
  begin
    wLinhaHeader := HTTPSend.Headers[I];
    if (Pos(aHeader, wLinhaHeader) > 0) then
    begin
      Result := Trim(Copy(wLinhaHeader, Length(aHeader)+1, Length(wLinhaHeader)));
      Break;
    end;
  end;
end;

function TACBrHTTP.GetProxyPass: String;
begin
  Result := fHTTPSend.ProxyPass;
end;

function TACBrHTTP.GetProxyPort: String;
begin
  Result := fHTTPSend.ProxyPort;
end;

function TACBrHTTP.GetProxyUser: String;
begin
  Result := fHTTPSend.ProxyUser;
end;

procedure TACBrHTTP.SetProxyHost(const AValue: String);
begin
  fHTTPSend.ProxyHost := AValue;
end;

procedure TACBrHTTP.SetProxyPass(const AValue: String);
begin
  fHTTPSend.ProxyPass := AValue;
end;

procedure TACBrHTTP.SetProxyPort(const AValue: String);
begin
  fHTTPSend.ProxyPort := AValue;
end;

procedure TACBrHTTP.SetProxyUser(const AValue: String);
begin
  fHTTPSend.ProxyUser := AValue;
end;

procedure TACBrHTTP.RegistrarLog(const aLinha: String);
var
  wTratado: Boolean;
begin
  wTratado := False;
  if Assigned(fOnQuandoGravarLog) then
    fOnQuandoGravarLog(aLinha, wTratado);

  if (not wTratado) and NaoEstaVazio(fArqLOG) then
    WriteLog(fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) + ' - ' + aLinha);
end;

end.
