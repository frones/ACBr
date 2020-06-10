{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior                                }
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
unit ACBrBoletoWS;

interface

uses
  Classes, SysUtils, ACBrBoleto, pcnGerador, pcnLeitor, ACBrUtil, pcnConversao, synacode, synautil,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}
    Jsons,
  {$EndIf}
  ACBrBoletoConversao, ACBrBoletoRetorno, ACBrDFeSSL, dateutils, strutils;

type

  EACBrBoletoWSException = class ( Exception );

  TBoletoWS = class;
  TRetornoEnvioClass = class;

  { TOAuth }
  TOAuth = class
  private
    FSSL : TDFeSSL;
    FURL : String;
    FContentType : String;
    FGrantType : String;
    FScope : String;
    FAmbiente : TpcnTipoAmbiente;
    FClientID : String;
    FClientSecret : String;
    FToken : String;
    FExpire : TDateTime;
    FErroComunicacao : String;

    procedure setURL(const AValue: String);
    procedure setContentType(const AValue: String);
    procedure setGrantType(const AValue: String);

    function getURL: String;
    function getContentType: String;
    function getGrantType: String;
    function getClientID : String;
    function getClientSecret : String;
    function getScope : String;

    procedure ProcessarRespostaOAuth(const ARetorno: AnsiString);
    function Executar(const AAuthBase64: String): Boolean;

  public
    constructor Create(ASSL: TDFeSSL; ATipoAmbiente: TpcnTipoAmbiente; AClientID, AClientSecret, AScope: String );
    destructor  Destroy; Override;

    function GerarToken: Boolean;

    property SSL : TDFeSSL read FSSL;
    property URL : String read getURL write setURL;
    property ContentType : String read getContentType write setContentType;
    property GrantType : String read getGrantType write setGrantType;
    property Scope : String read getScope;
    property ClientID : String read getClientID;
    property ClientSecret : String read getClientSecret;
    property Ambiente : TpcnTipoAmbiente read FAmbiente default taHomologacao;
    property Expire : TDateTime read FExpire;
    property ErroComunicacao : String read FErroComunicacao;
    property Token : String read FToken;

  end;

  { TBoletoWSClass }
  TBoletoWSClass = class
  private
    FDFeSSL: TDFeSSL;
    FBoletoWS: TBoletoWS;
    FGerador: TGerador;
    FBoleto: TACBrBoleto;
    FRetornoBanco: TRetornoEnvioClass;
    FRetornoWS: String;
    FTitulos: TACBrTitulo;
    FOAuth : TOAuth;

  protected
    FPDadosMsg: String;

  public
    constructor Create(ABoletoWS: TBoletoWS ); virtual;
    destructor  Destroy; Override;

    function ObterNomeArquivo: String; virtual;
    function GerarRemessa: String; virtual;
    function Enviar: Boolean; virtual;

    property Gerador: TGerador read FGerador;
    property Boleto: TACBrBoleto read FBoleto;
    property RetornoBanco: TRetornoEnvioClass read FRetornoBanco;
    property RetornoWS: String read FRetornoWS write FRetornoWS;
    property Titulos: TACBrTitulo read FTitulos write FTitulos;
    property DadosMsg: String read FPDadosMsg;
    property OAuth: TOAuth read FOAuth;
  end;

  { BoletoWS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TBoletoWS = class( TACBrWebService )
  private
    FBanco: TACBrTipoCobranca;
    FBoletoWSClass: TBoletoWSClass;
    FBoleto: TACBrBoleto;
    FRetornoBanco: TRetornoEnvioClass;
    FRetornoWS: String;
    procedure SetBanco(ABanco: TACBrTipoCobranca);

  public
    constructor Create(AOwner: TComponent);Override;
    destructor Destroy; override;
    procedure Clear;

    function ObterNomeArquivo: String;
    function GerarRemessa: String;
    function Enviar: Boolean; override;

    property Banco: TACBrTipoCobranca read FBanco write SetBanco;
    property BoletoWSClass: TBoletoWSClass read FBoletoWSClass;
    property Boleto: TACBrBoleto read FBoleto write FBoleto;
    property RetornoBanco: TRetornoEnvioClass read FRetornoBanco;
    property RetornoWS: String read FRetornoWS write FRetornoWS;

  end;

  { TRetornoEnvioClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRetornoEnvioClass = class
  private
    FACBrBoleto: TACBrBoleto;
    FRetWS: String;
    FCodRetorno: Integer;
    FMsg: String;
    FLeitor: TLeitor;

  public
    constructor Create(ABoletoWS: TACBrBoleto); virtual;
    destructor  Destroy; Override;
    function LerRetorno: Boolean; virtual;
    function RetornoEnvio: Boolean; virtual;

    property ACBrBoleto: TACBrBoleto read FACBrBoleto write FACBrBoleto;
    property RetWS: String read FRetWS write FRetWS;
    property Msg: String read FMsg write FMsg;
    property CodRetorno: Integer read FCodRetorno write FCodRetorno;
    property Leitor: TLeitor read FLeitor;

  end;

  { TBoletoWSSOAP }    //Bancos que utilizam XML
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TBoletoWSSOAP = class(TBoletoWSClass)
  private
    FPHeaderElement: String;

  protected
    FPSoapVersion: String;
    FPSoapEnvelopeAtributtes: String;
    FPEnvelopeSoap: String;
    FPURL: String;
    FPVersaoServico: String;
    FPServico : String;
    FPSoapAction : String;
    FPContentType : String;
    FPMimeType : String;
    FPRootElement: String;
    FPCloseRootElement: String;
    FPAuthorization : String;

    procedure DefinirEnvelopeSoap; virtual;
    procedure DefinirURL; virtual;
    procedure DefinirServicoEAction; virtual;
    procedure DefinirContentType; virtual;
    procedure DefinirMimeType; virtual;
    procedure DefinirRootElement; virtual;
    procedure DefinirAuthorization; virtual;

    procedure GerarHeader; virtual;
    procedure GerarDados; virtual;

    procedure Executar;

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: String; override;
    function Enviar: Boolean; override;

    property SoapVersion: String read FPSoapVersion;
    property SoapEnvelopeAtributtes: String read FPSoapEnvelopeAtributtes;
    property HeaderElement: String read FPHeaderElement;
    property RootElement: String read FPRootElement;
    property CloseRootElement: String read FPCloseRootElement;
    property EnvelopeSoap: String read FPEnvelopeSoap;
    property URL: String read FPURL;
    property VersaoServico: String read FPVersaoServico;
    property ContentType: String read FPContentType;
    property MimeType: String read FPMimeType;
    property Servico: String read FPServico;
    property SoapAction: String read FPSoapAction;
    property Authorization: String read FPAuthorization;

  end;

  { TRetornoEnvioSOAP }  //Retorno Bancos que utilizam XML
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRetornoEnvioSOAP = class(TRetornoEnvioClass)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); Override;

    function RetornoEnvio: Boolean; Override;
  end;

  { TBoletoWSREST }   //Implementar Bancos que utilizam JSON
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TBoletoWSREST = class(TBoletoWSClass)
  private

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: String; override;
    function Enviar: Boolean; override;

  end;

  { TRetornoEnvioREST }  //Implementar Retornos em JSON
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TRetornoEnvioREST = class(TRetornoEnvioClass)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); Override;

    function RetornoEnvio: Boolean; Override;
  end;



Const
  C_LER_RETORNO = 'Ler Retorno';
  C_RETORNO_ENVIO = 'Retorno Envio';
  C_DFESSL = 'DFeSSL';
  C_OBTER_NOME_ARQUIVO = 'Obter Nome Arquivo';
  C_GERAR_REMESSA = 'Gerar Remessa';
  C_ENVIAR = 'Enviar';
  C_REGISTRO_BOLETO = 'registro_boleto';
  C_DEFINIR_SERVICO_EACTION = 'DefinirServicoEAction';
  C_DEFINIR_URL = 'DefinirURL';
  c_DEFINIR_ENVELOPE_SOAP = 'DefinirEnvelopeSoap';
  C_GERAR_HEADER = 'GerarHeader';
  C_GERAR_DADOS = 'GerarDados';
  C_DEFINIR_ROOT_ELEMENT = 'RootElement';
  C_NO_CACHE = 'no-cache';
  C_GRANT_TYPE = 'grant_type';
  C_SCOPE = 'scope';
  C_CONTENT_TYPE = 'Content-Type';
  C_CACHE_CONTROL = 'Cache-Control';
  C_AUTHORIZATION = 'Authorization';

  C_RETORNO_REGISTRO = 'retorno_registro';
  C_ERRO = 'erro';
  C_ERROR_CODE = 'Error_Code ';
  C_HTTP_RESULT_CODE = 'HTTP_Result_Code ';

ResourceString
  S_METODO_NAO_IMPLEMENTADO =  'Metodo %s nao Implementado ';
  S_OPERACAO_NAO_IMPLEMENTADO =  'Operação %s nao Implementado para este Banco';
  S_ERRO_GERAR_TOKEN_AUTENTICACAO = 'Erro ao gerar token de Autenticação: %s';

implementation

uses
  ACBrBoletoW_Caixa, ACBrBoletoRet_Caixa, ACBrBoletoW_BancoBrasil, ACBrBoletoRet_BancoBrasil;

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

function TOAuth.getURL: String;
begin
  if FURL = '' then
    Raise Exception.Create(ACBrStr('Método de Autenticação inválido. URL não definida!'))
  else
    Result := FURL;
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
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    AJson: TJsonObject;
  {$Else}
    AJson: TJson;
  {$EndIf}
begin
  FToken := '';
  FExpire := 0;
  FErroComunicacao := '';

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonSerializationConfig.NullConvertsToValueTypes:=True;
    AJSon := TJsonObject.Parse(ARetorno) as TJsonObject;
    try
      if (FSSL.HTTPResultCode in [200, 201, 202]) then
      begin
        FToken := AJson.S['access_token'];
        try
          FExpire := Now + (AJson.I['expires_in'] * OneSecond);
        except
          FExpire:= 0;
        end;

      end
      else
        FErroComunicacao := 'HTTP_Code='+ IntToStr(FSSL.HTTPResultCode)
                           + ' Erro='+ AJson.S['error_description'];

    finally
      AJSon.Free;
    end;

  {$Else}
    AJSon := TJson.Create;
    try
      AJSon.Parse(ARetorno);
      if (FSSL.HTTPResultCode in [200, 201, 202]) then
      begin
        FToken := AJson.Values['access_token'].AsString;
        try
          FExpire := Now + (AJson.Values['expires_in'].AsNumber * OneSecond);
        except
          FExpire:= 0;
        end;

      end
      else
        FErroComunicacao := 'HTTP_Code='+ IntToStr(FSSL.HTTPResultCode)
                           + ' Erro='+ AJson.Values['error_description'].AsString;

    finally
      AJson.Free;
    end;

  {$EndIf}

end;

function TOAuth.Executar(const AAuthBase64: String): Boolean;
begin
  Result := False;
  FErroComunicacao := '';

  if not Assigned( TDFeSSL(FSSL) ) then
    raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_DFESSL] ));

  //Definindo Header da requisição OAuth
  FSSL.SSLHttpClass.Clear;
  with FSSL.SSLHttpClass.HeaderReq do
  begin
    Clear;
    Add(C_CONTENT_TYPE + ': ' + ContentType);
    Add(C_AUTHORIZATION + ': ' + AAuthBase64);
    Add(C_CACHE_CONTROL + ': ' + C_NO_CACHE);
  end;

  try
    //Utiliza HTTPMethod para envio
    FSSL.HTTPMethod('POST', URL
                            + '?' + C_GRANT_TYPE + '=' + GrantType
                            + '&' + C_SCOPE + '=' + Scope);

    FSSL.SSLHttpClass.DataResp.Position:= 0;
    ProcessarRespostaOAuth( ReadStrFromStream(FSSL.SSLHttpClass.DataResp, FSSL.SSLHttpClass.DataResp.Size ) );

    Result := (FErroComunicacao = '');
  except
    on E: Exception do
      raise EACBrBoletoWSException.Create(ACBrStr('Falha na Autenticação: '+ E.Message));

  end;

end;

constructor TOAuth.Create(ASSL: TDFeSSL; ATipoAmbiente: TpcnTipoAmbiente; AClientID, AClientSecret, AScope: String);
begin
  if Assigned( TDFeSSL(ASSL) ) then
    FSSL := TDFeSSL(ASSL);

  FAmbiente := ATipoAmbiente;
  FClientID := AClientID;
  FClientSecret := AClientSecret;
  FScope := AScope;
  FURL := '';
  FContentType := '';
  FGrantType := '';
  FToken := '';
  FExpire := 0;
  FErroComunicacao := '';

end;

destructor TOAuth.Destroy;
begin
  inherited Destroy;
end;

function TOAuth.GerarToken: Boolean;
begin
  Result := False;

  if ( Token <> '' ) and ( CompareDateTime( Expire, Now ) = 1 ) then                                        //Token ja gerado e ainda válido
    Result := True
  else                                                                                                      //Converte Basic da Autenticação em Base64
    Result := Executar( 'Basic ' + String(EncodeBase64(AnsiString(FClientID + ':' + FClientSecret))) );

end;

{ TRetornoEnvioREST }

constructor TRetornoEnvioREST.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvioREST.RetornoEnvio: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_RETORNO_ENVIO] ));
end;

{ TBoletoWSREST }

constructor TBoletoWSREST.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
end;

function TBoletoWSREST.GerarRemessa: String;
begin
  Result := '';
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_GERAR_REMESSA] ));
end;

function TBoletoWSREST.Enviar: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_ENVIAR] ));
end;

{ TRetornoEnvioSoap }

constructor TRetornoEnvioSOAP.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvioSOAP.RetornoEnvio: Boolean;
begin
  Result := False;
  leitor.Arquivo := ParseText(RetWS);
  LerRetorno;

  Result:= (FACBrBoleto.ListaRetornoWeb.Count > 0);

end;

{ TBoletoWSSOAP }

procedure TBoletoWSSOAP.DefinirEnvelopeSoap;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_DEFINIR_ENVELOPE_SOAP] ));
end;

procedure TBoletoWSSOAP.DefinirURL;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_DEFINIR_URL] ));
end;

procedure TBoletoWSSOAP.DefinirServicoEAction;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_DEFINIR_SERVICO_EACTION] ));
end;

procedure TBoletoWSSOAP.DefinirContentType;
begin
  if FPContentType = '' then
    FPContentType:= S_CONTENT_TYPE;
end;

procedure TBoletoWSSOAP.DefinirMimeType;
begin
  if FPMimeType = '' then
    FPMimeType:= S_MIME_TYPE;
end;

procedure TBoletoWSSOAP.DefinirRootElement;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_DEFINIR_ROOT_ELEMENT] ));
end;

procedure TBoletoWSSOAP.DefinirAuthorization;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_AUTHORIZATION] ));
end;

procedure TBoletoWSSOAP.GerarHeader;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_GERAR_HEADER] ));
end;

procedure TBoletoWSSOAP.GerarDados;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_GERAR_DADOS] ));
end;

procedure TBoletoWSSOAP.Executar;
var
  Stream: TMemoryStream;
begin
  try
    if FPAuthorization = '' then //Se Existir Autenticação deve utilizar HTTPMetod, anexando Token de Autorização no Header
      FRetornoWS:= FDFeSSL.Enviar(FPEnvelopeSoap, FPURL, FPSoapAction, FPMimeType )
    else
    begin
      try
        FDFeSSL.SSLHttpClass.Clear;
        FDFeSSL.SSLHttpClass.SoapAction:= FPSoapAction;
        FDFeSSL.SSLHttpClass.MimeType:= FPMimeType;
        with FDFeSSL.SSLHttpClass.HeaderReq do
        begin
          Clear;
          Add(FPAuthorization);
          Add(C_CONTENT_TYPE + ': ' + FPContentType);
        end;

        Stream:= TMemoryStream.Create;
        try
          WriteStrToStream(Stream, FPEnvelopeSoap);
          FDFeSSL.SSLHttpClass.DataReq.LoadFromStream(Stream);
          FDFeSSL.HTTPMethod('POST', FPURL);
        finally
          Stream.Free;
        end;

      finally
        FDFeSSL.SSLHttpClass.DataResp.Position:= 0;
        FRetornoWS:=  ReadStrFromStream(FDFeSSL.SSLHttpClass.DataResp, FDFeSSL.SSLHttpClass.DataResp.Size );

      end;
    end;

  finally
    FBoletoWS.FRetornoBanco.CodRetorno:= FDFeSSL.InternalErrorCode;
    FBoletoWS.FRetornoBanco.Msg := 'HTTP_Code='+ IntToStr(FDFeSSL.HTTPResultCode);

  end;

end;

constructor TBoletoWSSOAP.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPSoapVersion := S_SOAP_VERSION;
  FPMimeType := S_MIME_TYPE;
  FPContentType:= S_CONTENT_TYPE;
  FPHeaderElement := '';
  FPDadosMsg:= '';
  FPRootElement:= '';
  FPCloseRootElement:= '';
  FPEnvelopeSoap:= '';
  FPURL:= '';
  FPVersaoServico:= '';
  FPServico := '';
  FPAuthorization:= '';
  FPSoapAction := TipoOperacaoToStr(tpInclui);
  FPSoapEnvelopeAtributtes := C_SOAP_ATTRIBUTTES;
end;

function TBoletoWSSOAP.GerarRemessa: String;
begin
  Result:= '';
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';

  //Gera nameSpace Raiz do XML, implementado na classe do Banco selecionado
  DefinirRootElement;

  if NaoEstaVazio(FPRootElement) then
    Gerador.wGrupo(FPRootElement);

  //Gera o Cabeçalho XML, implementado na classe do Banco selecionado
  GerarHeader;
  //Gera os dados XML, implementado na classe do Banco selecionado
  GerarDados;

  if NaoEstaVazio(FPCloseRootElement) then
    Gerador.wGrupo('/' + FPCloseRootElement);

  FPDadosMsg := Gerador.ArquivoFormatoXML;

  Result := FPDadosMsg;

end;

function TBoletoWSSOAP.Enviar: Boolean;
begin
  Result := False;
  FBoletoWS.FRetornoBanco.CodRetorno:= 0;
  FBoletoWS.FRetornoBanco.Msg := '';
  FPAuthorization:= '';

  DefinirURL;
  DefinirEnvelopeSoap;
  FPEnvelopeSoap := UTF8ToNativeString(FPEnvelopeSoap);
  //Grava xml gerado
  if Boleto.Configuracoes.Arquivos.LogRegistro then
    WriteToTXT( FBoletoWS.ObterNomeArquivo, FPEnvelopeSoap , False, False);

  try
    Executar;
  finally
    Result := (FDFeSSL.HTTPResultCode in [200, 201, 202]);

    if Result then //Grava xml retorno
      WriteToTXT( ifthen( EstaVazio(Boleto.Configuracoes.Arquivos.PathGravarRegistro),
                  PathWithDelim( ApplicationPath ), PathWithDelim( Boleto.Configuracoes.Arquivos.PathGravarRegistro ))
                  + Titulos.NumeroDocumento +'-'+ C_RETORNO_REGISTRO + '.xml', FRetornoWS ,False, False);
  end;

end;

{ TRetornoEnvioClass }

constructor TRetornoEnvioClass.Create(ABoletoWS: TACBrBoleto);
begin
  FRetWS := '';
  FCodRetorno := 0;
  FMsg:= '';
  FLeitor  := TLeitor.Create;
  FACBrBoleto := ABoletoWS;

end;

destructor TRetornoEnvioClass.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TRetornoEnvioClass.LerRetorno: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_LER_RETORNO] ));

end;

function TRetornoEnvioClass.RetornoEnvio: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_RETORNO_ENVIO] ));

end;

{ TBoletoWSClass }

constructor TBoletoWSClass.Create(ABoletoWS: TBoletoWS);
begin
  FTitulos := Nil;
  FBoletoWS := ABoletoWS;
  FGerador := TGerador.Create;

  if Assigned( TDFeSSL(ABoletoWS.Boleto.Configuracoes.WebService) ) then
    FDFeSSL := TDFeSSL(ABoletoWS.Boleto.Configuracoes.WebService);

  FOAuth := TOAuth.Create(FDFeSSL,
                          FBoletoWS.Boleto.Configuracoes.WebService.Ambiente,
                          FBoletoWS.Boleto.Cedente.CedenteWS.ClientID,
                          FBoletoWS.Boleto.Cedente.CedenteWS.ClientSecret,
                          FBoletoWS.Boleto.Cedente.CedenteWS.Scope);

end;

destructor TBoletoWSClass.Destroy;
begin
  FGerador.Free;
  FOAuth.Free;
  inherited Destroy;
end;

function TBoletoWSClass.ObterNomeArquivo: String;
begin
  Result := '';
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_OBTER_NOME_ARQUIVO] ));
end;

function TBoletoWSClass.GerarRemessa: String;
begin
  Result := '';
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_GERAR_REMESSA] ));
end;

function TBoletoWSClass.Enviar: Boolean;
begin
  Result := False;
  raise EACBrBoletoWSException.Create(ClassName + Format( S_METODO_NAO_IMPLEMENTADO, [C_ENVIAR] ));
end;

{ TBoletoWS }

procedure TBoletoWS.SetBanco(ABanco: TACBrTipoCobranca);
begin
  if ABanco = FBanco then
    exit;

  if Assigned(FBoletoWSClass) then
    FreeAndNil(FBoletoWSClass);

  if Assigned(FRetornoBanco) then
    FreeAndNil(FRetornoBanco);

  case ABanco of
    cobCaixaEconomica:
      begin
        FBoletoWSClass := TBoletoW_Caixa.Create(Self);
        FRetornoBanco := TRetornoEnvio_Caixa.Create(FBoleto);
      end;
    cobBancoDoBrasil:
      begin
        FBoletoWSClass := TBoletoW_BancoBrasil.Create(Self);
        FRetornoBanco := TRetornoEnvio_BancoBrasil.Create(FBoleto);
      end;
  else
    FBoletoWSClass := TBoletoWSClass.Create(Self);
    FRetornoBanco := TRetornoEnvioClass.Create(FBoleto);

  end;

  FBoletoWSClass.FBoleto := FBoleto;

  FBanco := ABanco;

end;

constructor TBoletoWS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned( TACBrBoleto(AOwner) ) then
  begin
    FBoleto := TACBrBoleto(AOwner);
    Clear;
  end;

end;

procedure TBoletoWS.Clear;
begin
  if Assigned(FBoletoWSClass) then
    FBoletoWSClass.Free;

  FBoletoWSClass := TBoletoWSClass.Create(Self);

end;

destructor TBoletoWS.Destroy;
begin
  if Assigned(FBoletoWSClass) then
    FreeAndNil(FBoletoWSClass);

  if Assigned(FRetornoBanco) then
    FreeAndNil(FRetornoBanco);

  inherited Destroy;

end;

function TBoletoWS.ObterNomeArquivo: String;
var
  lPath: String;
begin
  if EstaVazio(Boleto.Configuracoes.Arquivos.PathGravarRegistro) then
    lPath := PathWithDelim( ApplicationPath )
  else
    lPath := PathWithDelim( Boleto.Configuracoes.Arquivos.PathGravarRegistro );

  if (Boleto.ListadeBoletos.Count > 0) then
    Result := lPath + FBoletoWSClass.Titulos.NumeroDocumento + '-'+ C_REGISTRO_BOLETO + '.xml'
  else
    Result := lPath + 'Rem' + FormatDateTime('ddmmyyhhnn',Now) +'-'+ C_REGISTRO_BOLETO + '.xml';

end;

function TBoletoWS.GerarRemessa: String;
var
  i: integer;
  lRetorno: String;
begin
  lRetorno:= '';
  Banco := FBoleto.Banco.TipoCobranca;

  if FBoleto.ListadeBoletos.Count > 0 then
  begin
    FBoleto.ListaRetornoWeb.Clear;
    for i:= 0 to FBoleto.ListadeBoletos.Count -1 do
    begin
      FBoletoWSClass.FTitulos := FBoleto.ListadeBoletos[i];
      FBoletoWSClass.GerarRemessa;
      if Boleto.Configuracoes.Arquivos.LogRegistro then
      begin
        WriteToTXT( ObterNomeArquivo, FBoletoWSClass.FPDadosMsg, False, False);
        lRetorno := lRetorno + sLineBreak
                  + ObterNomeArquivo;

      end;

    end;
    Result := lRetorno;
  end;
end;

function TBoletoWS.Enviar: Boolean;
var
  i: Integer;
begin
  Banco := FBoleto.Banco.TipoCobranca;
  Result := False;

  if FBoleto.ListadeBoletos.Count > 0 then
  begin
    FBoleto.ListaRetornoWeb.Clear;
    for i:= 0 to FBoleto.ListadeBoletos.Count -1 do
    begin
      FBoletoWSClass.FTitulos := FBoleto.ListadeBoletos[i];
      FBoletoWSClass.GerarRemessa;
      Result :=  FBoletoWSClass.Enviar;
      FRetornoWS := BoletoWSClass.FRetornoWS;

      FRetornoBanco.FRetWS:= FRetornoWS;
      FRetornoBanco.RetornoEnvio;
    end;

  end;

end;



end.

