{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Willian Delan, HelioNeto, Lucio Bittes,         }
{ Jhonlenon Ribeiro, rafabarzotto                                              }
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
//incluido em COLOCAR A DATA

{$I ACBr.inc}
unit ACBrBoletoW_Bradesco;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  Math,

  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  ACBrBoleto,
  ACBrBoletoWS.Rest,
  ACBrJSON,
  ACBrBoletoWS.SOAP;

type
  TEspecieDocumento = record
    Sigla: string;
    Codigo: Integer;
  end;
const
  TabelaEspecieDocumentos: array[1..32] of TEspecieDocumento = (
    (Sigla: 'CH'; Codigo: 1),
    (Sigla: 'DM'; Codigo: 2),
    (Sigla: 'DMI'; Codigo: 3),
    (Sigla: 'DS'; Codigo: 4),
    (Sigla: 'DSI'; Codigo: 5),
    (Sigla: 'DR'; Codigo: 6),
    (Sigla: 'LC'; Codigo: 7),
    (Sigla: 'NCC'; Codigo: 8),
    (Sigla: 'NCE'; Codigo: 9),
    (Sigla: 'NCI'; Codigo: 10),
    (Sigla: 'NCR'; Codigo: 11),
    (Sigla: 'NP'; Codigo: 12),
    (Sigla: 'NPR'; Codigo: 13),
    (Sigla: 'TM'; Codigo: 14),
    (Sigla: 'TS'; Codigo: 15),
    (Sigla: 'NS'; Codigo: 16),
    (Sigla: 'RC'; Codigo: 17),
    (Sigla: 'FAT'; Codigo: 18),
    (Sigla: 'ND'; Codigo: 19),
    (Sigla: 'AP'; Codigo: 20),
    (Sigla: 'ME'; Codigo: 21),
    (Sigla: 'PC'; Codigo: 22),
    (Sigla: 'DD'; Codigo: 23),
    (Sigla: 'CCB'; Codigo: 24),
    (Sigla: 'FI'; Codigo: 25),
    (Sigla: 'RD'; Codigo: 26),
    (Sigla: 'DRI'; Codigo: 27),
    (Sigla: 'EC'; Codigo: 28),
    (Sigla: 'ECI'; Codigo: 29),
    (Sigla: 'CC'; Codigo: 31),
    (Sigla: 'BDP'; Codigo: 32),
    (Sigla: 'OUT'; Codigo: 99)
  );
type

  { TBoletoW_Bradesco}
  TBoletoW_Bradesco = class(TBoletoWSREST)
    FUnixTime : Int64;
  private
    procedure AlteracaoDesconto(AJsonObject: TACBrJSONObject);
    procedure AlteraDataVencimento(AJsonObject: TACBrJSONObject);
    procedure AlterarEspecie(AJsonObject: TACBrJSONObject);
    procedure AlterarProtesto(AJsonObject: TACBrJSONObject);
    procedure AtribuirAbatimento(AJsonObject: TACBrJSONObject);
    procedure AtribuirDesconto(AJsonObject: TACBrJSONObject);
    function DateTimeToDateBradesco( const AValue:TDateTime ):String;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros: String;
    procedure DefinirParamOAuth; override;
    procedure DefinirKeyUser;
    function ValidaAmbiente: Integer;

    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsultaDetalhe;

    procedure GerarBenificiarioFinalHibrido(AJsonObject: TACBrJSONObject);
    procedure GerarBenificiarioFinalComum(AJsonObject: TACBrJSONObject);
    procedure GerarJuros(AJsonObject: TACBrJSONObject);
    procedure GerarMulta(AJsonObject: TACBrJSONObject);
    procedure GerarDesconto(AJsonObject: TACBrJSONObject);
    procedure RegistraHibrido;
    procedure RegistraComum;
    procedure DefinirURLAmbiente(const AUseCert: Boolean);

  public
    constructor Create(ABoletoWS: TBoletoWS; AACBrBoleto : TACBrBoleto); reintroduce;
    function GerarRemessa: string; override;
    function Enviar: boolean; override;
    function AgenciaContaFormatada(const APadding : Integer = 11) : String;
    function EspecieDocumento : Integer;
  end;

const
  {LEGADO - PDF}
  C_URL_LEGADO      = 'https://openapi.bradesco.com.br';
  C_URL_HOM_LEGADO  = 'https://proxy.api.prebanco.com.br';

  C_URL_OAUTH_PROD_LEGADO  = 'https://openapi.bradesco.com.br/auth/server/v%s/token';
  C_URL_OAUTH_HOM_LEGADO   = 'https://proxy.api.prebanco.com.br/auth/server/v%s/token';

  {PORTAL DEVELOPERS}
  C_URL_PORTAL            = 'https://openapi.bradesco.com.br';
  C_URL_OAUTH_PROD_PORTAL = 'https://openapi.bradesco.com.br/auth/server-mtls/v2/token';


  C_URL_HOM_PORTAL       = 'https://openapisandbox.prebanco.com.br';
  C_URL_OAUTH_HOM_PORTAL = 'https://openapisandbox.prebanco.com.br/auth/server-mtls/v2/token';

  // Portal (com certificado)
  PATH_PIX_PORTAL         = '/boleto-hibrido/cobranca-registro/v1/gerarBoleto';
  PATH_COB_PORTAL         = '/boleto/cobranca-registro/v1/cobranca';
  PATH_BAIXAR_PORTAL      = '/boleto/cobranca-registro/v1/titulo-baixar';
  PATH_ALTVENC_PORTAL     = '/boleto/cobranca-registro/v1/alterar-titulo';
  PATH_CONSULTA_PORTAL    = '/boleto/cobranca-registro/v1/titulo-consultar';

  // Legado (sem certificado)
  PATH_COB_LEGADO         = '/v1/boleto-hibrido/registrar-boleto';
  PATH_BAIXAR_LEGADO      = '/v1/boleto/titulo-baixar';
  PATH_ALTVENC_LEGADO     = '/v1/boleto/alterar-titulo';
  PATH_CONSULTA_LEGADO    = '/v1/boleto/titulo-consultar';




  C_ACCEPT          = '*/*';
  C_AUTHORIZATION   = 'Authorization';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'UTF-8';

implementation

uses
  synacode,
  httpsend,

  ACBrDFeSSL,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base,
  pcnAuxiliar,
  ACBrBoletoWS.Rest.OAuth,
  ACBr.Auth.JWT;

{ TBoletoW_Bradesco}

procedure TBoletoW_Bradesco.DefinirURL;
var
  LPath: string;
  LUseCert: Boolean;
  LIndicadorPix: Boolean;
begin
  LUseCert := Boleto.Configuracoes.WebService.UseCertificateHTTP;
  LIndicadorPix := Boleto.Cedente.CedenteWS.IndicadorPix;

  // Configura URLs base conforme ambiente
  if LUseCert then
  begin
    case Boleto.Configuracoes.WebService.Ambiente of
      tawsProducao:    FPURL.URLProducao    := C_URL_PORTAL;
      tawsHomologacao: FPURL.URLHomologacao := C_URL_HOM_PORTAL;
    end;
  end else
  begin
    case Boleto.Configuracoes.WebService.Ambiente of
      tawsProducao:    FPURL.URLProducao    := C_URL_LEGADO;
      tawsHomologacao: FPURL.URLHomologacao := C_URL_HOM_LEGADO;
    end;
  end;

  // Define o path conforme operação e certificado
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        if LUseCert then
        begin
          if LIndicadorPix then
            LPath := PATH_PIX_PORTAL
          else
            LPath := PATH_COB_PORTAL;
        end else
          LPath := PATH_COB_LEGADO;
      end;
    tpAltera:
      begin
        case ATitulo.OcorrenciaOriginal.Tipo of
          ACBrBoleto.toRemessaBaixar:
            if LUseCert then
              LPath := PATH_BAIXAR_PORTAL
            else
              LPath := PATH_BAIXAR_LEGADO;

          ACBrBoleto.toRemessaAlterarVencimento:
            if LUseCert then
              LPath := PATH_ALTVENC_PORTAL
            else
              LPath := PATH_ALTVENC_LEGADO;
        else
          raise Exception.Create('Tipo de ocorrência não suportado para alteração.');
        end;
      end;
    tpConsultaDetalhe:
      begin
        if LUseCert then
          LPath := PATH_CONSULTA_PORTAL
        else
          LPath := PATH_CONSULTA_LEGADO;
      end;

    tpBaixa:
      begin
        if LUseCert then
          LPath := PATH_BAIXAR_PORTAL
        else
          LPath := PATH_BAIXAR_LEGADO;
      end;
  end;
  FPURL.SetPathURI(LPath);
end;

procedure TBoletoW_Bradesco.DefinirURLAmbiente(const AUseCert: Boolean);
begin
  if AUseCert then
  begin
    FPURL.URLProducao    := C_URL_PORTAL;
    FPURL.URLHomologacao := C_URL_HOM_PORTAL;
  end else
  begin
    FPURL.URLProducao    := C_URL_LEGADO;
    FPURL.URLHomologacao := C_URL_HOM_LEGADO;
  end;
end;

procedure TBoletoW_Bradesco.DefinirContentType;
begin
  if OAuth.Token = '' then
    FPContentType := 'application/x-www-form-urlencoded'
  else
    FPContentType := 'application/json';
end;

procedure TBoletoW_Bradesco.GerarHeader;
var
  LDataAtual: TDateTime;
  LIntMiliSegundos: Int64;
  LStrTimeStamp:string ;
  LStrRequestAssinado: string;
  LStrConteudo:string;
  LMetodoURI : String;
  LJWTAuth : TACBrJWTAuth;
  LStringList : TStringList;
  LSignature : string;
  LTimestamp : string;

  function addLine(const AString : String; const ABreakLine : Boolean = True) : String;
  begin
    Result := Trim(AString);
    if ABreakLine then
      Result := Result + AnsiChar(#10);
  end;

begin
  if Boleto.Configuracoes.WebService.UseCertificateHTTP then
  begin  // NOVO PORTAL DEVELOPERS
    ClearHeaderParams;
    AddHeaderParam('Accept-Encoding', 'gzip, deflate, compress');
    DefinirContentType;
  end else
  begin  // LEGADO
    if OAuth.Token = '' then
      GerarTokenAutenticacao;

    LTimestamp := ACBrUtil.DateTime.DateTimeTodhUTC(ACBrUtil.DateTime.DateTimeUniversalToLocal(ACBrUtil.DateTime.GetUTCSistema,UnixToDateTime(FUnixTime)), ACBrUtil.DateTime.GetUTCSistema);

    ClearHeaderParams;
    DefinirContentType;
    DefinirKeyUser;
    if FPDadosMsg <> '' then
    begin

       LMetodoURI := FPURL.GetURL;
       if Boleto.Configuracoes.WebService.Ambiente = tawsProducao then
         LMetodoURI := StringReplace(LMetodoURI, C_URL_LEGADO, '', [rfReplaceAll, rfIgnoreCase])
       else
         LMetodoURI := StringReplace(LMetodoURI, C_URL_HOM_LEGADO, '', [rfReplaceAll, rfIgnoreCase]);

       LStrConteudo :=  {1}   addLine(MetodoHTTPToStr(FMetodoHTTP))
                        {2} + addLine(LMetodoURI)
                        {3} + addLine('')
                        {4} + addLine(FPDadosMsg)
                        {5} + addLine(OAuth.Token)
                        {6} + addLine(IntToStr(FUnixTime * 1000))
                        {7} + addLine(LTimestamp)
                        {8} + addLine('SHA256',False);

      LStringList := TStringList.Create;

      try
        LStringList.LoadFromFile(Boleto.Configuracoes.WebService.ArquivoKEY);
        LJWTAuth := TACBrJWTAuth.Create(LStringList.Text);
        try
          LSignature := LJWTAuth.Signature(LStrConteudo);
        finally
          LJWTAuth.Free;
        end;
      finally
        LStringList.Free;
      end;
      AddHeaderParam('X-Brad-Signature', LSignature);
      AddHeaderParam('X-Brad-Nonce', IntToStr(FUnixTime * 1000) );
      AddHeaderParam('X-Brad-Timestamp', LTimestamp );
      AddHeaderParam('X-Brad-Algorithm', 'SHA256');
      AddHeaderParam('acess-token',Boleto.Cedente.CedenteWS.ClientID);
      AddHeaderParam('cpf-cnpj',OnlyNumber(Boleto.Cedente.CNPJCPF) )
    end;
  end;
end;

procedure TBoletoW_Bradesco.GerarDados;
begin
  if Assigned(Boleto) then
    DefinirURL;
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
    begin
      FMetodoHTTP := htPOST;//Define Método POST para Incluir.
      RequisicaoJson;
      GerarHeader//Necessário o Json do boleto para usar no Header na assinatura.
    end;
    tpAltera:
    begin
      FMetodoHTTP := htPATCH;//Define Método PATCH para Alteração.
      RequisicaoAltera;
    end;
    tpBaixa:
    begin
      FMetodoHTTP := htPOST;//Define Método POST para Baixa.
      RequisicaoBaixa;
      GerarHeader;
    end;
    tpConsultaDetalhe:
    begin
      FMetodoHTTP := htPOST;//Define Método POST Consulta Detalhe.
      RequisicaoConsultaDetalhe;
      GerarHeader;
    end;
  else
    raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
                                        [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;
end;

procedure TBoletoW_Bradesco.DefinirAuthorization;
begin
  //PORTAL
  if Boleto.Configuracoes.WebService.UseCertificateHTTP then
    FPAuthorization := C_Authorization + ': ' + 'Bearer ' + GerarTokenAutenticacao
  else //LEGADO
    FPAuthorization := Format( '%s: Bearer %s',[C_Authorization , GerarTokenAutenticacao] );
end;

function TBoletoW_Bradesco.GerarTokenAutenticacao: string;
const
  PARAMS_OAUTH = '';
var
  LVersao : String;
  LJSonObject : TACBrJSONObject;
begin
  OAuth.Payload := True;
  Result:= '';
  if Boleto.Configuracoes.WebService.UseCertificateHTTP then // novo portal developers
  begin
    case OAuth.Ambiente of
      tawsProducao    : OAuth.URL.URLProducao    := C_URL_OAUTH_PROD_PORTAL;
      tawsHomologacao : OAuth.URL.URLHomologacao := C_URL_OAUTH_HOM_PORTAL;
    end;
    OAuth.AuthorizationType := atBearer;
    Result := inherited GerarTokenAutenticacao;
  end else
  begin //LEGADO

    FUnixTime := DateTimeToUnix(ACBrUtil.DateTime.DateTimeUniversal(ACBrUtil.DateTime.GetUTCSistema,Now));

    if OAuth.Ambiente = tawsProducao then
    begin
      OAuth.URL.URLProducao := C_URL_OAUTH_PROD_LEGADO;
      LVersao := '1.1';
    end else
    if OAuth.Ambiente = tawsHomologacao then
    begin
      OAuth.URL.URLHomologacao := C_URL_OAUTH_HOM_LEGADO;
      if Boleto.Cedente.CedenteWS.IndicadorPix then
        LVersao := '1.2'
      else
        LVersao := '1.1';
    end;

    case Boleto.Configuracoes.WebService.Ambiente of
      tawsProducao: OAuth.URL.URLProducao := Format(OAuth.URL.URLProducao,['1.1']); //página 7;
      tawsHomologacao: OAuth.URL.URLHomologacao := Format(OAuth.URL.URLHomologacao,[LVersao]);
    end;

    if Assigned(OAuth) then
    begin
      OAuth.AuthorizationType := atJWT;
      OAuth.GrantType   := 'urn:ietf:params:oauth:grant-type:jwt-bearer';
      try
        LJSonObject := TACBrJSONObject.Create
                       .AddPair('aud',OAuth.URL.GetURL)
                       .AddPair('sub',Trim(Boleto.Cedente.CedenteWS.ClientID))
                       .AddPair('iat',FUnixTime - 3600 )
                       .AddPair('exp',FUnixTime + 3600)
                       .AddPair('jti',FUnixTime * 1000)
                       .AddPair('ver',LVersao);
        OAuth.ParamsOAuth := LJSonObject.ToJSON;
      finally
        LJSonObject.Free;
      end;

      OAuth.AddHeaderParam('Accept-Encoding', C_ACCEPT_ENCODING);
      OAuth.AddHeaderParam('Accept-Charset' , C_ACCEPT_CHARSET);
      OAuth.AddHeaderParam('Accept','*/*');
      if OAuth.GerarToken then
        Result := OAuth.Token
      else
        raise EACBrBoletoWSException.Create(ClassName + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO, [OAuth.ErroComunicacao] ));
    end;
  end;
end;

procedure TBoletoW_Bradesco.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Bradesco.DefinirParametros: String;
var
  LConsulta: TStringList;
begin
  if not Assigned(Boleto.Configuracoes.WebService.Filtro) then
    Exit;
  LConsulta := TStringList.Create;
  try
    LConsulta.Delimiter := '&';
    LConsulta.Add( 'agencia='+Boleto.Cedente.Agencia);
    LConsulta.Add( 'conta='+Boleto.Cedente.Conta);
    result := LConsulta.DelimitedText;
  finally
    LConsulta.Free;
  end;
end;

procedure TBoletoW_Bradesco.DefinirParamOAuth;
begin
  if Boleto.Configuracoes.WebService.UseCertificateHTTP then // novo portal developers
    FParamsOAuth := Format( 'grant_type=%s&client_id=%s&client_secret=%s', ['client_credentials',  Boleto.Cedente.CedenteWS.ClientID, Boleto.Cedente.CedenteWS.ClientSecret] )
  else //LEGADO
    FParamsOAuth := Format( 'grant_type=%s&assertion=%s', ['urn:ietf:params:oauth:grant-type:jwt-bearer', FPKeyUser] );
end;

function TBoletoW_Bradesco.DateTimeToDateBradesco(const AValue: TDateTime): String;
begin
  Result := FormatDateBr(AValue, 'DD.MM.YYYY');
end;

function TBoletoW_Bradesco.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao, '1','2'), 2);
end;

procedure TBoletoW_Bradesco.RequisicaoBaixa;
var
  LJsonObject, LJsonCnpjCPF: TACBrJSONObject;
begin
  if not Assigned(ATitulo) then
    Exit;

  LJsonObject := TACBrJSONObject.Create;
  LJsonCnpjCPF := TACBrJSONObject.Create;
  try
    LJsonCnpjCPF.AddPair('cpfCnpj', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
    LJsonCnpjCPF.AddPair('filial', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
    LJsonCnpjCPF.AddPair('controle', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2));
    LJsonObject.AddPair('cpfCnpj', LJsonCnpjCPF);
    LJsonObject.AddPair('produto', RemoveZerosEsquerda(ATitulo.Carteira));
    LJsonObject.AddPair('negociacao', AgenciaContaFormatada(11));
    LJsonObject.AddPair('nossoNumero', OnlyNumber(ATitulo.NossoNumero));
    LJsonObject.AddPair('sequencia', '0');
    LJsonObject.AddPair('codigoBaixa', '57');

    FPDadosMsg := LJsonObject.ToJSON;
  finally
    LJsonObject.Free;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoJson;
begin
  if Boleto.Cedente.CedenteWS.IndicadorPix then
    RegistraHibrido
  else
    RegistraComum;
end;

procedure TBoletoW_Bradesco.RegistraComum;
var
  LJsonObject: TACBrJSONObject;
begin
  LJsonObject := TACBrJSONObject.Create;
  try
    LJsonObject.AddPair('nuCPFCNPJ',    Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
    LJsonObject.AddPair('filialCPFCNPJ',Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
    LJsonObject.AddPair('ctrlCPFCNPJ', IfThen(Boleto.Cedente.TipoInscricao = pJuridica,
                                        Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2),
                                        Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 10, 2)));
    LJsonObject.AddPair('idProduto', ATitulo.Carteira);
    LJsonObject.AddPair('nuNegociação', AgenciaContaFormatada(18));
    LJsonObject.AddPair('nuTítulo', OnlyNumber(ATitulo.NossoNumero));
    LJsonObject.AddPair('nuCliente', Trim(IfThen(ATitulo.NumeroDocumento <> '',
                                           ATitulo.NumeroDocumento,
                                           IfThen(ATitulo.SeuNumero <> '',
                                                  ATitulo.SeuNumero,
                                                  OnlyNumber(ATitulo.NossoNumero)))));
    LJsonObject.AddPair('dtEmissãoTítulo', DateTimeToDateBradesco(ATitulo.DataDocumento));
    LJsonObject.AddPair('dtVencimentoTítulo', DateTimeToDateBradesco(ATitulo.Vencimento));
    LJsonObject.AddPair('tpVencimento', 0);//FIXO.
    LJsonObject.AddPair('vlNominalTítulo', ATitulo.ValorDocumento*100);
    LJsonObject.AddPair('cdEspécieTítulo', EspecieDocumento);


    //LJsonObject.AddPair('tpProtestoAutomáticoNegativação', );
    //LJsonObject.AddPair('prazoProtestoAutomáticoNegativação', );
    LJsonObject.AddPair('controleParticipante', Trim(IfThen(ATitulo.NumeroDocumento <> '',
                                           ATitulo.NumeroDocumento,
                                           IfThen(ATitulo.SeuNumero <> '',
                                                  ATitulo.SeuNumero,
                                                  OnlyNumber(ATitulo.NossoNumero)))));



   // LJsonObject.AddPair('cdPagamentoParcial', );
    //LJsonObject.AddPair('qtdePagamentoParcial', );

    GerarJuros(LJsonObject);
    GerarMulta(LJsonObject);

    GerarDesconto(LJsonObject);



    //LJsonObject.AddPair('prazoBonificação', 0);
    //LJsonObject.AddPair('percentualBonificação', 0);
    //LJsonObject.AddPair('vlBonificação', 0);
    //LJsonObject.AddPair('dtLimiteBonificação', );
    LJsonObject.AddPair('vlAbatimento',  ATitulo.ValorAbatimento * 100);
    LJsonObject.AddPair('vlIOF', ATitulo.ValorIOF * 100);





    LJsonObject.AddPair('nomePagador', Copy(TiraAcentos(ATitulo.Sacado.NomeSacado), 1, 70));
    LJsonObject.AddPair('logradouroPagador', Copy(TiraAcentos(ATitulo.Sacado.Logradouro) + ' ' +ATitulo.Sacado.Numero, 1, 40));
    LJsonObject.AddPair('NuLogradouroPagador', 0);//FIXO
    LJsonObject.AddPair('cepPagador', Copy(OnlyNumber(ATitulo.Sacado.CEP), 1, 5));
    LJsonObject.AddPair('complementoCepPagador', Copy(OnlyNumber(ATitulo.Sacado.CEP), 6, 8));
    LJsonObject.AddPair('bairroPagador', Copy(TiraAcentos(ATitulo.Sacado.Bairro), 1, 40));
    LJsonObject.AddPair('municípioPagador', Copy(TiraAcentos(ATitulo.Sacado.Cidade), 1, 30));
    LJsonObject.AddPair('ufPagador', Copy(TiraAcentos(ATitulo.Sacado.UF), 1, 2));
    LJsonObject.AddPair('cdIndCpfcnpjPagador', IfThen(ATitulo.Sacado.Pessoa = pJuridica, '2', '1'));
    LJsonObject.AddPair('nuCpfcnpjPagador', OnlyNumber(ATitulo.Sacado.CNPJCPF));

    GerarBenificiarioFinalComum(LJsonObject);

    FPDadosMsg := LJsonObject.ToJSON;

  finally
    LJsonObject.Free;
  end;
end;

procedure TBoletoW_Bradesco.RegistraHibrido;
var
  LJsonObject: TACBrJSONObject;
begin
  if not Assigned(ATitulo) then
    Exit;
  LJsonObject := TACBrJSONObject.Create;
  try
    LJsonObject.AddPair('registrarTitulo', 1); //1 = Registrar o título 2 = Somente consistir dados do título
    LJsonObject.AddPair('codUsuario', 'APISERVIC');//FIXO.
    LJsonObject.AddPair('nroCpfCnpjBenef', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
    LJsonObject.AddPair('filCpfCnpjBenef', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
    LJsonObject.AddPair('digCpfCnpjBenef', IfThen(Boleto.Cedente.TipoInscricao = pJuridica,
                                           Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2),
                                           Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 10, 2)));
    LJsonObject.AddPair('tipoAcesso', 2);//FIXO.
    LJsonObject.AddPair('cpssoaJuridContr', 0);//FIXO.
    LJsonObject.AddPair('ctpoContrNegoc', 0);//FIXO.
    LJsonObject.AddPair('nseqContrNegoc', 0);//FIXO.
    LJsonObject.AddPair('cidtfdProdCobr', RemoveZerosEsquerda(ATitulo.Carteira));
    LJsonObject.AddPair('cnegocCobr', AgenciaContaFormatada(18));
    LJsonObject.AddPair('codigoBanco', 237);//FIXO.
    LJsonObject.AddPair('filler', '');//FIXO.
    LJsonObject.AddPair('eNseqContrNegoc', 0);//FIXO.
    //tipoRegistro: 1-Título 2-Título com Instrução de Protesto 3-Título com Instrução de Protesto Falimentar.
    LJsonObject.AddPair('tipoRegistro', 1);//NÃO Obrigatório;
    LJsonObject.AddPair('cprodtServcOper', 0);//FIXO.
    LJsonObject.AddPair('ctitloCobrCdent', OnlyNumber(ATitulo.NossoNumero));//NÃO Obrigatório;

    //ctitloCliCdent: Identificador do título pelo beneficiário(Seu Número).
    LJsonObject.AddPair('ctitloCliCdent', Trim(IfThen(ATitulo.NumeroDocumento <> '',
                                           ATitulo.NumeroDocumento,
                                           IfThen(ATitulo.SeuNumero <> '',
                                                  ATitulo.SeuNumero,
                                                  OnlyNumber(ATitulo.NossoNumero)))));
    LJsonObject.AddPair('demisTitloCobr', DateTimeToDateBradesco(ATitulo.DataDocumento));
    LJsonObject.AddPair('dvctoTitloCobr', DateTimeToDateBradesco(ATitulo.Vencimento));
    LJsonObject.AddPair('cidtfdTpoVcto', 0);//FIXO.
    if Boleto.Configuracoes.WebService.UseCertificateHTTP then // Portal Developers
      LJsonObject.AddPair('cindcdEconmMoeda', '9')
    else // LEGADO
      LJsonObject.AddPair('cindcdEconmMoeda', '00006');
    LJsonObject.AddPair('vnmnalTitloCobr', ATitulo.ValorDocumento*100);
    LJsonObject.AddPair('qmoedaNegocTitlo', 0);//FIXO.
    LJsonObject.AddPair('cespceTitloCobr', EspecieDocumento);
    LJsonObject.AddPair('cindcdAceitSacdo', 'N');
   //ctpoProteTitlo: Tipo de protesto automático do título: 1 = Dias corridos | 2 = Dias úteis.
    LJsonObject.AddPair('ctpoProteTitlo', 0);//NÃO Obrigatório;
    //Quantidade de dias após o vencimento, para protesto automático. Obrigatório? Sim, caso informado ctpoProteTitlo.
    LJsonObject.AddPair('ctpoPrzProte', 0);
    //Tipo decurso de protesto: 1 = Dias corridos | 2 = Dias úteis. Obrigatório? Sim, caso informado ctpoProteTitlo.
    LJsonObject.AddPair('ctpoProteDecurs', 0);
    LJsonObject.AddPair('ctpoPrzDecurs', 0);//FIXO.
    LJsonObject.AddPair('cctrlPartcTitlo', 0);//NÃO Obrigatório;
    if Boleto.Configuracoes.WebService.UseCertificateHTTP then // Portal Developers
      LJsonObject.AddPair('cformaEmisPplta', '02')//FIXO.
    else
      LJsonObject.AddPair('cformaEmisPplta', 2);//FIXO. // LEGADO

    LJsonObject.AddPair('cindcdPgtoParcial', 'N');//FIXO.
    LJsonObject.AddPair('qtdePgtoParcial', 000);//FIXO.
    LJsonObject.AddPair('filler1', '');//FIXO.

    GerarJuros(LJsonObject);
    GerarMulta(LJsonObject);
    GerarDesconto(LJsonObject);

    //Tipo de prazo desconto/bonificação: 1 = Dias corridos | 2 = Dias úteis. Obrigatório? Sim, caso informado valor ou percentual de desconto/bonificação.
    LJsonObject.AddPair('ctpoPrzCobr', 0);
    if Boleto.Configuracoes.WebService.UseCertificateHTTP then
    begin
      LJsonObject.AddPair('pdescBonifPgto', '');//NÃO Obrigatório;
      LJsonObject.AddPair('vdescBonifPgto', '');//NÃO Obrigatório;
    end else
    begin //LEGADO
      LJsonObject.AddPair('pdescBonifPgto', 0);//NÃO Obrigatório;
      LJsonObject.AddPair('vdescBonifPgto', 0);//NÃO Obrigatório;
    end;

    LJsonObject.AddPair('dlimBonifPgto', '');// 'Exemplo 01.01.2001';Caso informado o acima.
    LJsonObject.AddPair('vabtmtTitloCobr', 0);//NÃO Obrigatório;
    LJsonObject.AddPair('viofPgtoTitlo', 0);//NÃO Obrigatório;
    LJsonObject.AddPair('filler2', '');//FIXO.
    LJsonObject.AddPair('isacdoTitloCobr', Copy(TiraAcentos(ATitulo.Sacado.NomeSacado), 1, 70));
    LJsonObject.AddPair('elogdrSacdoTitlo', Copy(TiraAcentos(ATitulo.Sacado.Logradouro), 1, 40));
    LJsonObject.AddPair('enroLogdrSacdo', ATitulo.Sacado.Numero);
    LJsonObject.AddPair('ecomplLogdrSacdo', Copy(TiraAcentos(ATitulo.Sacado.Complemento), 1, 15));
    LJsonObject.AddPair('ccepSacdoTitlo', Copy(OnlyNumber(ATitulo.Sacado.CEP), 1, 5));
    LJsonObject.AddPair('ccomplCepSacdo', Copy(OnlyNumber(ATitulo.Sacado.CEP), 6, 8));
    LJsonObject.AddPair('ebairoLogdrSacdo', Copy(TiraAcentos(ATitulo.Sacado.Bairro), 1, 40));
    LJsonObject.AddPair('imunSacdoTitlo', Copy(TiraAcentos(ATitulo.Sacado.Cidade), 1, 30));
    LJsonObject.AddPair('csglUfSacdo', Copy(TiraAcentos(ATitulo.Sacado.UF), 1, 2));
    LJsonObject.AddPair('indCpfCnpjSacdo', IfThen(ATitulo.Sacado.Pessoa = pJuridica, '2', '1'));
    LJsonObject.AddPair('nroCpfCnpjSacdo', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    LJsonObject.AddPair('renderEletrSacdo', Copy(ATitulo.Sacado.Email, 1, 70));
    LJsonObject.AddPair('cdddFoneSacdo', Copy(OnlyNumber(ATitulo.Sacado.Fone), 1, 3));//NÃO Obrigatório;
    LJsonObject.AddPair('cfoneSacdoTitlo', Copy(OnlyNumber(ATitulo.Sacado.Fone), 1, 11));//NÃO Obrigatório;
    LJsonObject.AddPair('bancoDeb', 0);//Código do Banco para débito automático. -> NÃO Obrigatório;
    LJsonObject.AddPair('agenciaDeb', 0);//Número da agência para débito automático. -> NÃO Obrigatório;
    LJsonObject.AddPair('agenciaDebDv', 0);//Dígito verificador da Agência para débito automático. -> Caso informado agenciaDeb;
    LJsonObject.AddPair('contaDeb', 0);//Número da conta para débito automático. -> Caso informado agenciaDeb;
    LJsonObject.AddPair('bancoCentProt', 0);//FIXO.
    LJsonObject.AddPair('agenciaDvCentPr', 0);//FIXO.

    GerarBenificiarioFinalHibrido(LJsonObject);
    if Boleto.Configuracoes.WebService.UseCertificateHTTP then // PORTAL DEVELOPERS
      LJsonObject.AddPair('filler3', 0)//FIXO.
    else
      LJsonObject.AddPair('filler3', '');//FIXO. //LEGADO
    LJsonObject.AddPair('fase', 1);//FIXO.
    LJsonObject.AddPair('cindcdCobrMisto', 'S');//FIXO.
    LJsonObject.AddPair('ialiasAdsaoCta', '');//Chave Pix do beneficiário. Manter em branco.
    LJsonObject.AddPair('iconcPgtoSpi', '');//TXID do título. Manter em branco.
    LJsonObject.AddPair('caliasAdsaoCta', '');//Códigos de erro na geração do QR Code pelo BSPI. Manter em branco.
    LJsonObject.AddPair('ilinkGeracQrcd', '');//Identificação do location do QR Code gerado pelo BSPI. Manter em branco.
    LJsonObject.AddPair('wqrcdPdraoMercd', '');//Código EMV do QR Code gerado pelo BSPI. Manter em branco.
    LJsonObject.AddPair('validadeAposVencimento', '');//Quantidade de dias após vencimento, que o título é válido para pagamento via Pix. Manter em branco.
    LJsonObject.AddPair('filler4', '');//Manter em branco.
    if Boleto.Configuracoes.WebService.UseCertificateHTTP then
      LJsonObject.AddPair('idLoc', '');
    FPDadosMsg := LJsonObject.ToJSON;
  finally
    LJsonObject.Free;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoAltera;
var
  LJsonObject: TACBrJSONObject;
begin
  if not Assigned(ATitulo) then
    Exit;
  LJsonObject := TACBrJSONObject.Create;
  try
    LJsonObject.AddPair('numeroContrato',ATitulo.ACBrBoleto.Cedente.CodigoCedente);
    LJsonObject.AddPair('modalidade',ATitulo.ACBrBoleto.Cedente.Modalidade);
    LJsonObject.AddPair('nossoNumero',OnlyNumber(ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo)));

    case Integer(ATitulo.ACBrBoleto.ListadeBoletos.Objects[0].OcorrenciaOriginal.Tipo) of
       1 : // Baixa
        begin
          LJsonObject.AddPair('seuNumero', IfThen(ATitulo.SeuNumero <> '',
                                                    ATitulo.SeuNumero,
                                                    IfThen(ATitulo.NumeroDocumento <> '',
                                                      ATitulo.NumeroDocumento,
                                                      OnlyNumber(ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo))
                                                    )
                                                  ));
        end;
      5: //RemessaConcederDesconto
        begin
          AtribuirDesconto(LJsonObject);
        end;
      7: //RemessaAlterarVencimento
        begin
          AlteraDataVencimento(LJsonObject);
        end;
      9:  //RemessaProtestar
        begin
          FMetodoHTTP := htPOST;
          AlterarProtesto(LJsonObject);
        end;
      10:  //RemessaSustarProtesto
        begin
          FMetodoHTTP :=  htDELETE;
          AlterarProtesto(LJsonObject);
        end;
      37: //RemessaCobrarJurosMora
        begin
          FMetodoHTTP := htPOST;
          GerarJuros(LJsonObject);
        end;
      50:  //RemessaAlterarMulta
        begin
          FMetodoHTTP := htPOST;
          GerarMulta(LJsonObject);
        end;
      52: //RemessaAlterarDesconto
        begin
          FMetodoHTTP := htPOST;
          AlteracaoDesconto(LJsonObject);
        end;
      54: //RemessaAlterarAbatimento
        begin
          FMetodoHTTP := htPOST;
          AtribuirAbatimento(LJsonObject);
        end;
      64:  //Alterar Especie
        begin
          FMetodoHTTP := htPOST;
          AlterarEspecie(LJsonObject);
        end;
    end;

    FPDadosMsg := Format('[%s]',[LJsonObject.ToJSON]);
  finally
    LJsonObject.Free;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoConsultaDetalhe;
var
  LJsonObject, LJsonCpfCnpj: TACBrJSONObject;
begin
  if not Assigned(ATitulo) then
    Exit;
  LJsonObject := TACBrJSONObject.Create;
  try
    LJsonCpfCnpj := TACBrJSONObject.Create;
    LJsonCpfCnpj.AddPair('cpfCnpj', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
    LJsonCpfCnpj.AddPair('filial', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
    LJsonCpfCnpj.AddPair('controle', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2));
    LJsonObject.AddPair('cpfCnpj', LJsonCpfCnpj);

    LJsonObject.AddPair('produto', RemoveZerosEsquerda(ATitulo.Carteira));
    LJsonObject.AddPair('negociacao', AgenciaContaFormatada(11));
    LJsonObject.AddPair('nossoNumero', OnlyNumber(ATitulo.NossoNumero));
    LJsonObject.AddPair('sequencia', '0');
    LJsonObject.AddPair('status', 0);

    FPDadosMsg := LJsonObject.ToJSON;
  finally
    LJsonObject.Free;
  end;
end;

procedure TBoletoW_Bradesco.GerarBenificiarioFinalComum(AJsonObject: TACBrJSONObject);
var LTipoPessoa : String;
begin
  if not Assigned(ATitulo) or not Assigned(AJsonObject) then
    Exit;
  case ATitulo.Sacado.SacadoAvalista.Pessoa of
    pFisica   : LTipoPessoa := '1';
    pJuridica : LTipoPessoa := '2';
    else
      LTipoPessoa := '0';
  end;
  AJsonObject.AddPair('nomeSacadorAvalista', Copy(TiraAcentos(ATitulo.Sacado.SacadoAvalista.NomeAvalista), 1, 40));
  AJsonObject.AddPair('logradouroSacadorAvalista', Copy(TiraAcentos(ATitulo.Sacado.SacadoAvalista.Logradouro) + ' '+ATitulo.Sacado.SacadoAvalista.Numero, 1, 40));
  AJsonObject.AddPair('cepSacadorAvalista', Copy(ATitulo.Sacado.SacadoAvalista.CEP, 1, 5));
  AJsonObject.AddPair('complementoCepSacadorAvalista', Copy(ATitulo.Sacado.SacadoAvalista.CEP, 6, 8));
  AJsonObject.AddPair('bairroSacadorAvalista', Copy(TiraAcentos(ATitulo.Sacado.SacadoAvalista.Bairro), 1, 40));
  AJsonObject.AddPair('municípioSacadorAvalista', Copy(TiraAcentos(ATitulo.Sacado.SacadoAvalista.Cidade), 1, 40));
  AJsonObject.AddPair('ufSacadorAvalista', Copy(TiraAcentos(ATitulo.Sacado.SacadoAvalista.UF), 1, 2));
  AJsonObject.AddPair('cdIndCpfcnpjSacadorAvalista', LTipoPessoa);
  AJsonObject.AddPair('nuCpfcnpjSacadorAvalista', Copy(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF), 1, 14));
  AJsonObject.AddPair('endEletrônicoSacadorAvalista', Copy(ATitulo.Sacado.SacadoAvalista.Email, 1, 70));
end;

procedure TBoletoW_Bradesco.GerarBenificiarioFinalHibrido(AJsonObject: TACBrJSONObject);
  procedure ValorPadrao(out AVar : string; const ADefault : string = '0');
  begin
    if Trim(AVar) = '' then
      AVar := ADefault;
  end;
var LCEP, LCEPComplemento, LTelefoneDDD, LTelefone,
  LDocumento, LNumero : String;
begin
  if not Assigned(ATitulo) or not Assigned(AJsonObject) then
    Exit;
  LCEP := Copy(aTitulo.Sacado.SacadoAvalista.CEP, 1, 5);
  LCEPComplemento := Copy(aTitulo.Sacado.SacadoAvalista.CEP, 6, 8);
  LTelefoneDDD := Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.Fone), 1, 3);
  LTelefone := Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.Fone), 1, 11);
  LDocumento := Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF), 1, 14);
  LNumero := aTitulo.Sacado.SacadoAvalista.Numero;
  if Boleto.Configuracoes.WebService.UseCertificateHTTP then
  begin
    ValorPadrao(LCEP);
    ValorPadrao(LCEPComplemento);
    ValorPadrao(LTelefoneDDD);
    ValorPadrao(LTelefone);
    ValorPadrao(LDocumento);
    ValorPadrao(LNumero);
  end;
  AJsonObject.AddPair('isacdrAvalsTitlo', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.NomeAvalista), 1, 40));
  AJsonObject.AddPair('nroCpfCnpjSacdr', LDocumento);
  AJsonObject.AddPair('ccepSacdrTitlo', LCEP);
  AJsonObject.AddPair('ccomplCepSacdr', LCEPComplemento);
  AJsonObject.AddPair('elogdrSacdrAvals', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Logradouro), 1, 10));
  AJsonObject.AddPair('enroLogdrSacdr', LNumero);
  AJsonObject.AddPair('ecomplLogdrSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Complemento), 1, 15));
  AJsonObject.AddPair('ebairoLogdrSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Bairro), 1, 40));
  AJsonObject.AddPair('imunSacdrAvals', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Cidade), 1, 40));
  AJsonObject.AddPair('csglUfSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.UF), 1, 2));
  AJsonObject.AddPair('indCpfCnpjSacdr', IfThen(aTitulo.Sacado.SacadoAvalista.Pessoa = pJuridica, '2', '1'));
  AJsonObject.AddPair('renderEletrSacdr', Copy(aTitulo.Sacado.SacadoAvalista.Email, 1, 70));
  AJsonObject.AddPair('cdddFoneSacdr', LTelefoneDDD);
  AJsonObject.AddPair('cfoneSacdrTitlo', LTelefone);

end;

procedure TBoletoW_Bradesco.GerarJuros(AJsonObject: TACBrJSONObject);
var
  LPercentualTitulo, LValorTitulo, LDiaTitulo: string;
  LCodigoMora, LDiasJuros: Integer;
begin
  if not Assigned(ATitulo) or not Assigned(AJsonObject) then
    Exit;

  // Ajustar código de mora se vazio
  if ATitulo.CodigoMora = '' then
  begin
    case ATitulo.CodigoMoraJuros of
      cjValorDia:   ATitulo.CodigoMora := '1';
      cjTaxaMensal: ATitulo.CodigoMora := '2';
      cjIsento:     ATitulo.CodigoMora := '3';
    else
      ATitulo.CodigoMora := '3';
    end;
  end;

  LCodigoMora := StrToIntDef(ATitulo.CodigoMora, 0);

  // Calcular dias
  if ATitulo.ValorMoraJuros > 0 then
    LDiasJuros := DaysBetween(ATitulo.Vencimento, ATitulo.DataMoraJuros)
  else
    LDiasJuros := 0;

  if ATitulo.ACBrBoleto.Cedente.CedenteWS.IndicadorPix then
  begin
    LPercentualTitulo  := 'ptxJuroVcto';
    LValorTitulo := 'vdiaJuroMora';
    LDiaTitulo  := 'qdiaInicJuro';
  end
  else
  begin
    LPercentualTitulo  := 'percentualJuros';
    LValorTitulo := 'vlJurosc';
    LDiaTitulo  := 'qtdeDiasJuros';
  end;

  // Preencher JSON de forma genérica
  case LCodigoMora of
    0, 3: // Isento
      begin
        AJsonObject.AddPair(LPercentualTitulo, 0);
        AJsonObject.AddPair(LValorTitulo, 0);
        AJsonObject.AddPair(LDiaTitulo, 0);
      end;

    1: // Valor ao dia
      begin
        AJsonObject.AddPair(LValorTitulo, ATitulo.ValorMoraJuros * 100);
        AJsonObject.AddPair(LDiaTitulo, LDiasJuros);
        AJsonObject.AddPair(LPercentualTitulo, 0);
      end;

    2: // Percentual ao mês
      begin
        AJsonObject.AddPair(LPercentualTitulo, ATitulo.ValorMoraJuros * 100);
        AJsonObject.AddPair(LDiaTitulo, LDiasJuros);
        AJsonObject.AddPair(LValorTitulo, 0);
      end;
  end;
end;

procedure TBoletoW_Bradesco.GerarMulta(AJsonObject: TACBrJSONObject);
var
  LPercentualTitulo, LValorTitulo, LDiasTitulo: string;
  LCodMulta: Integer;
  LDataMulta: TDateTime;
  LDiasMulta: Integer;
begin
  if not Assigned(ATitulo) or not Assigned(AJsonObject) then
    Exit;

  // Define os nomes dos campos de acordo com o indicador Pix
  if ATitulo.ACBrBoleto.Cedente.CedenteWS.IndicadorPix then
  begin
    LPercentualTitulo := 'pmultaAplicVcto';
    LValorTitulo      := 'vmultaAtrsoPgto';
    LDiasTitulo       := 'qdiaInicMulta';
  end
  else
  begin
    LPercentualTitulo := 'percentualMulta';
    LValorTitulo      := 'vlMulta';
    LDiasTitulo       := 'qtdeDiasMulta';
  end;

  // Define código do tipo de multa
  if ATitulo.PercentualMulta > 0 then
  begin
    if ATitulo.MultaValorFixo then
      LCodMulta := 1
    else
      LCodMulta := 2;
  end
  else
    LCodMulta := 3;

  // Data base para cálculo dos dias
  if ATitulo.DataMulta > 0 then
    LDataMulta := ATitulo.DataMulta
  else
    LDataMulta := ATitulo.DataMoraJuros;

  // Dias para início de multa
  if (LCodMulta <> 3) and (ATitulo.PercentualMulta > 0) then
    LDiasMulta := DaysBetween(ATitulo.Vencimento, LDataMulta)
  else
    LDiasMulta := 0;

  // Adiciona os dados ao JSON
  case LCodMulta of
    1: // Valor fixo
      begin
        AJsonObject.AddPair(LPercentualTitulo, 0);
        AJsonObject.AddPair(LValorTitulo, ATitulo.PercentualMulta * 100);
        AJsonObject.AddPair(LDiasTitulo, LDiasMulta);
      end;
    2: // Percentual
      begin
        AJsonObject.AddPair(LPercentualTitulo, ATitulo.PercentualMulta);
        AJsonObject.AddPair(LValorTitulo, 0);
        AJsonObject.AddPair(LDiasTitulo, LDiasMulta);
      end;
    3: // Isento
      begin
        AJsonObject.AddPair(LPercentualTitulo, 0);
        AJsonObject.AddPair(LValorTitulo, 0);
        AJsonObject.AddPair(LDiasTitulo, 0);
      end;
  end;
end;

constructor TBoletoW_Bradesco.Create(ABoletoWS: TBoletoWS; AACBrBoleto : TACBrBoleto);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;
  if Assigned(OAuth) then
  begin
    OAuth.Payload := True;
    OAuth.ContentType       := 'application/x-www-form-urlencoded';
    OAuth.AuthorizationType := atJWT;
  end;
end;

function TBoletoW_Bradesco.GerarRemessa: string;
begin
  DefinirCertificado;
  Result := inherited GerarRemessa;
end;

function TBoletoW_Bradesco.Enviar: boolean;
begin
  DefinirCertificado;
  Result := inherited Enviar;
end;

function TBoletoW_Bradesco.EspecieDocumento: Integer;
var
  I: Integer;
begin
  for I := Low(TabelaEspecieDocumentos) to High(TabelaEspecieDocumentos) do
  begin
    if SameText(TabelaEspecieDocumentos[I].Sigla, ATitulo.EspecieDoc) then
    begin
      Result := TabelaEspecieDocumentos[I].Codigo;
      Exit;
    end;
    Result := StrToIntDef(ATitulo.EspecieDoc,0);
  end;
end;

procedure TBoletoW_Bradesco.GerarDesconto(AJsonObject: TACBrJSONObject);
var
  LPercentual, LValor, LData: string;
  i, TipoDesconto: Integer;

  procedure AdicionarDescontoJSON(const Sufixo: string; TipoDesconto: Integer);
  begin
    if TipoDesconto <> 0 then
    begin
      if TipoDesconto = 1 then
      begin
        AJsonObject.AddPair(LPercentual + Sufixo, 0);
        AJsonObject.AddPair(LValor + Sufixo, ATitulo.ValorDesconto);
      end
      else
      begin
        AJsonObject.AddPair(LPercentual + Sufixo, ATitulo.ValorDesconto);
        AJsonObject.AddPair(LValor + Sufixo, 0);
      end;
      AJsonObject.AddPair(LData + Sufixo, DateTimeToDateBradesco(ATitulo.Vencimento));
    end
    else
    begin
      AJsonObject.AddPair(LPercentual + Sufixo, 0);
      AJsonObject.AddPair(LValor + Sufixo, 0);
      AJsonObject.AddPair(LData + Sufixo, '');
    end;
  end;

begin
  if not Assigned(ATitulo) or not Assigned(AJsonObject) then
    Exit;

  // Escolha dos campos dependendo do indicador PIX
  if ATitulo.ACBrBoleto.Cedente.CedenteWS.IndicadorPix then
  begin
    LPercentual := 'pdescBonifPgto0';
    LValor      := 'vdescBonifPgto0';
    LData       := 'dlimDescBonif';
  end
  else
  begin
    LPercentual := 'percentualDesconto';
    LValor      := 'vlDesconto';
    LData       := 'dataLimiteDesconto';
  end;

  for i := 1 to 3 do
  begin
    case i of
      1: TipoDesconto := Integer(ATitulo.TipoDesconto);
      2: TipoDesconto := Integer(ATitulo.TipoDesconto2);
      3: TipoDesconto := Integer(ATitulo.TipoDesconto3);
    end;

    AdicionarDescontoJSON(IntToStr(i), TipoDesconto);
  end;
end;

procedure TBoletoW_Bradesco.AlteraDataVencimento(AJsonObject: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) and (ATitulo.Vencimento > 0) then
    AJsonObject.AddPair('dataVencimento',DateTimeToDateBradesco(aTitulo.Vencimento));
end;

procedure TBoletoW_Bradesco.AtribuirAbatimento(AJsonObject: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) and (ATitulo.Vencimento > 0) then
    AJsonObject.AddPair('valorAbatimento',aTitulo.ValorAbatimento);
end;

procedure TBoletoW_Bradesco.AlterarEspecie(AJsonObject: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) and (ATitulo.Vencimento > 0) then
    AJsonObject.AddPair('especieDocumento',EspecieDocumento);
end;

procedure TBoletoW_Bradesco.AtribuirDesconto(AJsonObject: TACBrJSONObject);
begin
  if Assigned(aTitulo) and Assigned(AJsonObject) then
    GerarDesconto(AJsonObject);
end;

function TBoletoW_Bradesco.AgenciaContaFormatada(const APadding : Integer) : String;
var
  LAgencia, LConta, LZeros, LPadding : String;
begin
  LConta := RemoveZerosEsquerda(ATitulo.ACBrBoleto.Cedente.Conta);
  LAgencia := ATitulo.ACBrBoleto.Cedente.Agencia;

  LZeros := Poem_Zeros('0',APadding - (Length(LAgencia) + Length(LConta)));

  Result := LAgencia + LZeros + LConta;
end;

procedure TBoletoW_Bradesco.AlteracaoDesconto(AJsonObject: TACBrJSONObject);
begin
 if Assigned(ATitulo) and Assigned(AJsonObject) then
   GerarDesconto(AJsonObject);
end;

procedure TBoletoW_Bradesco.AlterarProtesto(AJsonObject: TACBrJSONObject);
begin
  // Sem Payload
end;

end.

