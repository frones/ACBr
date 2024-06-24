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
unit ACBrBoletoWS.Rest;

interface

uses
  ACBrOpenSSLUtils,
  httpsend,
  ACBrJSON,
  SysUtils,
  DateUtils,
  Classes,
  ACBrBoletoConversao,
  pcnConversao,
  ACBrBoletoWS,
  ACBrBoleto,
  ACBrBase;

type
    { TBoletoWSREST }   //Implementar Bancos que utilizam JSON
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(pidWin32 or pidWin64) ]
{$ENDIF RTL230_UP}

  TBoletoWSREST = class(TBoletoWSClass)
  private

  protected
    FPURL          : String;
    FPContentType  : String;
    FPKeyUser      : String;
    FPIdentificador: String;
    FPAccept       : String;
    FPAuthorization: String;
    FMetodoHTTP    : TMetodoHTTP;
    FParamsOAuth   : String;
    FPHeaders      : TStringList;
    procedure setDefinirAccept(const AValue: String);
    procedure setMetodoHTTP(const AValue: TMetodoHTTP);
    procedure DefinirAuthorization; virtual;
    procedure DefinirContentType; virtual;
    procedure DefinirURL; virtual;
    procedure DefinirCertificado;
    procedure DefinirProxy;

    procedure GerarHeader; virtual;
    procedure GerarDados; virtual;

    function GerarTokenAutenticacao: String; virtual;
    function GerarRemessa: String; override;
    function Enviar: Boolean; override;
    procedure DefinirParamOAuth; virtual;
    procedure Executar;

  public
    constructor Create(ABoletoWS: TBoletoWS); override;
    destructor Destroy; override;

  end;

    { TRetornoEnvioREST }  //Implementar Retornos em JSON
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(pidWin32 or pidWin64) ]
{$ENDIF RTL230_UP}

  TRetornoEnvioREST = class(TRetornoEnvioClass)
  private

  protected
      //FSucessResponse: Boolean;
    function RetornoEnvio(const AIndex: Integer): Boolean; Override;

  public
    constructor Create(ABoletoWS: TACBrBoleto); Override;

  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrBoletoWS.Rest.OAuth,
  ACBrUtil.Base,
  synautil,
  synacode,
  StrUtils,
  ACBrCompress,
  ACBrUtil.FilesIO;

  { TRetornoEnvioREST }

constructor TRetornoEnvioREST.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
    //FSucessResponse:= False;
end;

function TRetornoEnvioREST.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  if (ACBrBoleto.ListadeBoletos.Count > 0) and (ACBrBoleto.Configuracoes.WebService.Operacao <> tpConsulta) then
  begin
    Result := LerRetorno(ACBrBoleto.ListadeBoletos[ AIndex ].RetornoWeb);
    ACBrBoleto.ListadeBoletos[ AIndex ].QrCode; //GetQRCode valida campos no titulo
  end
  else
    Result := LerListaRetorno;

end;

  { TBoletoWSREST }

procedure TBoletoWSREST.DefinirCertificado;
begin
  BoletoWS.ArquivoCRT   := Boleto.Configuracoes.WebService.ArquivoCRT;
  BoletoWS.ArquivoKEY   := Boleto.Configuracoes.WebService.ArquivoKEY;
  BoletoWS.ChavePrivada := Boleto.Configuracoes.WebService.ChavePrivada;
  BoletoWS.Certificado  := Boleto.Configuracoes.WebService.Certificado;

    // Adicionando o chave privada
  if NaoEstaVazio(BoletoWS.ChavePrivada) then
  begin
    if StringIsPEM(BoletoWS.ChavePrivada) then
      httpsend.Sock.SSL.PrivateKey := ConvertPEMToASN1(BoletoWS.ChavePrivada)
    else
      httpsend.Sock.SSL.PrivateKey := BoletoWS.ChavePrivada;
  end
  else
    if NaoEstaVazio(BoletoWS.ArquivoKEY) then
      httpsend.Sock.SSL.PrivateKeyFile := BoletoWS.ArquivoKEY;

  if NaoEstaVazio(BoletoWS.Certificado) then
  begin
    if StringIsPEM(BoletoWS.Certificado) then
      httpsend.Sock.SSL.Certificate := ConvertPEMToASN1(BoletoWS.Certificado)
    else
      httpsend.Sock.SSL.Certificate := BoletoWS.Certificado;
  end
  else
    if NaoEstaVazio(BoletoWS.ArquivoCRT) then
      httpsend.Sock.SSL.CertificateFile := BoletoWS.ArquivoCRT;
end;

procedure TBoletoWSREST.DefinirContentType;
begin
  if FPContentType = '' then
    FPContentType := S_CONTENT_TYPE;
end;

procedure TBoletoWSREST.DefinirParamOAuth;
begin
  DefinirCertificado;
  FParamsOAuth := C_GRANT_TYPE + '=' + OAuth.GrantType + '&' + C_SCOPE + '=' + OAuth.Scope;
end;

procedure TBoletoWSREST.DefinirProxy;
begin
  httpsend.ProxyHost := BoletoWS.ProxyHost;
  httpsend.ProxyPort := BoletoWS.ProxyPort;
  httpsend.ProxyUser := BoletoWS.ProxyUser;
  httpsend.ProxyPass := BoletoWS.ProxyPass;

  if (BoletoWS.TimeOut <> 0) then
    httpsend.TimeOut := BoletoWS.TimeOut;
end;

procedure TBoletoWSREST.setDefinirAccept(const AValue: String);
begin
  if AValue <> '' then
    FPAccept := AValue;
end;

procedure TBoletoWSREST.setMetodoHTTP(const AValue: TMetodoHTTP);
begin
  FMetodoHTTP := AValue;
end;

procedure TBoletoWSREST.DefinirURL;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_DEFINIR_URL ]));
end;

destructor TBoletoWSREST.Destroy;
begin
  FPHeaders.Free;
  inherited;
end;

procedure TBoletoWSREST.GerarHeader;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_GERAR_HEADER ]));
end;

procedure TBoletoWSREST.GerarDados;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_GERAR_DADOS ]));
end;

procedure TBoletoWSREST.DefinirAuthorization;
begin
  raise EACBrBoletoWSException.Create(ClassName + Format(S_METODO_NAO_IMPLEMENTADO, [ C_AUTHORIZATION ]));
end;

function TBoletoWSREST.GerarTokenAutenticacao: String;
begin
  Result := '';
  if Assigned(OAuth) then
  begin
    BoletoWS.DoLog('Autenticando Token... ', logSimples);
    DefinirParamOAuth;
    OAuth.ParamsOAuth := FParamsOAuth;
    if OAuth.GerarToken then
      Result := OAuth.Token
    else
      BoletoWS.DoLog(Format(S_ERRO_GERAR_TOKEN_AUTENTICACAO, [ OAuth.ErroComunicacao ]) ,logSimples);
  end;
end;

procedure TBoletoWSREST.Executar;
var
  LHeaders: TStringList;
  LStream : TStringStream;
  LCT     : TCompressType;
begin
  LStream  := TStringStream.Create('');
  LHeaders := TStringList.Create;
  try
    httpsend.OutputStream := LStream;
    httpsend.Headers.Clear;

    if FPAccept <> '' then
      LHeaders.Add(C_ACCEPT + ': ' + FPAccept);

    if FPAuthorization <> '' then
      LHeaders.Add(FPAuthorization);

    if FPKeyUser <> '' then
      LHeaders.Add(FPKeyUser);

    if FPIdentificador <> '' then
      LHeaders.Add(FPIdentificador);

      //if FPContentType <> '' then
      //  LHeaders.Add(C_CONTENT_TYPE +': '+ FPContentType);

    httpsend.Headers.AddStrings(LHeaders);

    if FPHeaders.Count > 0 then
      httpsend.Headers.AddStrings(FPHeaders);

    httpsend.MimeType := FPContentType;
  finally
    LHeaders.Free;
  end;
  httpsend.Document.Clear;
  try
    httpsend.Document.Position := 0;
    if FPDadosMsg <> '' then
      WriteStrToStream(httpsend.Document, NativeStringToUTF8(FPDadosMsg));

    BoletoWS.DoLog('URL: [' + MetodoHTTPToStr(FMetodoHTTP) + '] ' + FPURL, logSimples);
    BoletoWS.DoLog('Header:', logParanoico);
    BoletoWS.DoLog(httpsend.Headers.Text, logParanoico);

    httpsend.HTTPMethod(MetodoHTTPToStr(FMetodoHTTP), FPURL);
  finally
    httpsend.Document.Position := 0;

    try
      if LStream.Size > 0 then
      begin
        LCT := DetectCompressType(LStream);
        if (LCT = ctUnknown) then // Not compressed...
        begin
          LStream.Position := 0;
          FRetornoWS       := ReadStrFromStream(LStream, LStream.Size);
        end
        else
          FRetornoWS := UnZip(LStream);
      end;
    except
      LStream.Position := 0;
      FRetornoWS       := ReadStrFromStream(LStream, LStream.Size);
    end;

    FRetornoWS                       := String(UTF8ToNativeString(FRetornoWS));
    BoletoWS.RetornoBanco.CodRetorno := httpsend.Sock.LastError;
    try
      BoletoWS.RetornoBanco.Msg            := Trim('HTTP_Code=' + IntToStr(httpsend.ResultCode) + ' ' + httpsend.ResultString + ' ' + FRetornoWS);
      BoletoWS.RetornoBanco.HTTPResultCode := httpsend.ResultCode;
    finally
      LStream.Free;
    end;
  end;
end;

constructor TBoletoWSREST.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FTipoRegistro   := C_JSON;
  FMetodoHTTP     := htPOST;
  FPContentType   := '';
  FPAccept        := '';
  FPDadosMsg      := '';
  FPURL           := '';
  FPAuthorization := '';
  FPKeyUser       := '';
  FPIdentificador := '';
  FPHeaders       := TStringList.Create;
end;

function TBoletoWSREST.GerarRemessa: String;
begin
  Result := '';
  httpsend.Headers.Clear;
    //Gera o Header, para REST
  GerarHeader;
    //Gera o Json, implementado na classe do Banco selecionado
  GerarDados;

  Result := FPDadosMsg;
end;

function TBoletoWSREST.Enviar: Boolean;
begin
  BoletoWS.RetornoBanco.CodRetorno := 0;
  BoletoWS.RetornoBanco.Msg        := '';

  DefinirAuthorization;
  DefinirURL;
  DefinirContentType;
  DefinirCertificado;
  DefinirProxy;

  //Grava json gerado
  BoletoWS.DoLog('Comando Enviar: ' + ClassName, logSimples);
  BoletoWS.DoLog('Comando Enviar: ' + FPDadosMsg, logSimples);

  try
    Executar;
  finally
    Result := (BoletoWS.RetornoBanco.HTTPResultCode in [ 200 .. 207 ]);

    BoletoWS.DoLog('Retorno Envio: ' + Self.ClassName, logSimples);
    BoletoWS.DoLog('Código do Envio: ' + IntToStr(BoletoWS.RetornoBanco.HTTPResultCode) + ' ' + httpsend.Protocol + ' ' +  httpsend.ResultString, logSimples);

    if Result then //Grava retorno
      BoletoWS.DoLog('Retorno Envio: ' + FRetornoWS, logParanoico)
    else
      BoletoWS.DoLog('Retorno Envio: ' + IfThen(BoletoWS.RetornoBanco.CodRetorno > 0,
          sLineBreak + 'ErrorCode=' + IntToStr(BoletoWS.RetornoBanco.CodRetorno), '') + sLineBreak + 'Result=' + NativeStringToAnsi(FRetornoWS), logParanoico);

    BoletoWS.DoLog('Cookies:', logParanoico);
    BoletoWS.DoLog(httpsend.Cookies.Text, logParanoico);
    BoletoWS.DoLog(httpsend.Sock.SSL.CertificateFile, logParanoico);
    BoletoWS.DoLog(httpsend.Sock.SSL.PrivateKeyFile, logParanoico);
    BoletoWS.DoLog('Header:', logParanoico);
    BoletoWS.DoLog(httpsend.Headers.Text, logParanoico);
  end;
end;

end.
