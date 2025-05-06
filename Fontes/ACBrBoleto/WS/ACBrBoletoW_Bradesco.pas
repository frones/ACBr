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

    procedure DefinirAutenticacao;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsultaDetalhe;

    procedure GerarBenificiarioFinal(AJsonObject: TACBrJSONObject);
    procedure GerarJuros(AJsonObject: TACBrJSONObject);
    procedure GerarMulta(AJsonObject: TACBrJSONObject);
    procedure GerarDesconto(AJsonObject: TACBrJSONObject);

  public
    constructor Create(ABoletoWS: TBoletoWS; AACBrBoleto : TACBrBoleto); reintroduce;
    function GerarRemessa: string; override;
    function Enviar: boolean; override;
    function AgenciaContaFormatada(const APadding : Integer = 11) : String;
    function EspecieDocumento : Integer;
  end;

const
  C_URL             = 'https://openapi.bradesco.com.br';
  C_URL_HOM         = 'https://proxy.api.prebanco.com.br';

  C_URL_OAUTH_PROD  = 'https://openapi.bradesco.com.br/auth/server/v%s/token';
  C_URL_OAUTH_HOM   = 'https://proxy.api.prebanco.com.br/auth/server/v%s/token';


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
begin
  case Boleto.Configuracoes.WebService.Ambiente of
    tawsProducao    : FPURL.URLProducao    := C_URL;
    tawsHomologacao : FPURL.URLHomologacao := C_URL_HOM;
  end;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui: FPURL.SetPathURI( '/v1/boleto-hibrido/registrar-boleto' );
    tpAltera:
    begin
       if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaBaixar then
         FPURL.SetPathURI( '/v1/boleto/titulo-baixar' )
       else if aTitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarVencimento then
         FPURL.SetPathURI( '/v1/boleto/alterar-titulo' );
    end;
    tpConsultaDetalhe:  FPURL.SetPathURI( '/v1/boleto/titulo-consultar' );
    tpBaixa:  FPURL.SetPathURI( '/v1/boleto/titulo-baixar' );
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
       LMetodoURI := StringReplace(LMetodoURI, C_URL, '', [rfReplaceAll, rfIgnoreCase])
     else
       LMetodoURI := StringReplace(LMetodoURI, C_URL_HOM, '', [rfReplaceAll, rfIgnoreCase]);

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
  FPAuthorization := Format( '%s: Bearer %s',[C_Authorization , GerarTokenAutenticacao] );
end;

function TBoletoW_Bradesco.GerarTokenAutenticacao: string;
const PARAMS_OAUTH = '';
var
  LVersao : String;
  LJSonObject : TACBrJSONObject;
begin
  OAuth.Payload := True;
  result:= '';

  //FUnixTime := DateTimeToUnix(Now, False);
  FUnixTime := DateTimeToUnix(ACBrUtil.DateTime.DateTimeUniversal(ACBrUtil.DateTime.GetUTCSistema,Now));

  if OAuth.Ambiente = tawsProducao then
  begin
    OAuth.URL.URLProducao := C_URL_OAUTH_PROD;
    LVersao := '1.1';
  end else
  if OAuth.Ambiente = tawsHomologacao then
  begin
    OAuth.URL.URLHomologacao := C_URL_OAUTH_HOM;
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
      result := OAuth.Token
    else
      raise EACBrBoletoWSException.Create(ClassName + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO, [OAuth.ErroComunicacao] ));
  end;
end;

procedure TBoletoW_Bradesco.DefinirKeyUser;
begin
  if Assigned(aTitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Bradesco.DefinirParametros: String;
var
  LConsulta: TStringList;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
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
end;

procedure TBoletoW_Bradesco.DefinirParamOAuth;
begin
  FParamsOAuth := Format( 'grant_type=%s&assertion=%s', ['urn:ietf:params:oauth:grant-type:jwt-bearer', FPKeyUser] );
end;

function TBoletoW_Bradesco.DateTimeToDateBradesco(const AValue: TDateTime): String;
begin
   result := FormatDateBr(AValue, 'DD.MM.YYYY');
end;

function TBoletoW_Bradesco.ValidaAmbiente: Integer;
begin
  result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao, '1','2'), 2);
end;

procedure TBoletoW_Bradesco.DefinirAutenticacao;
begin
  FPAuthorization := Format( '%s: %s', [C_ACCESS_TOKEN , GerarTokenAutenticacao]);
end;

procedure TBoletoW_Bradesco.RequisicaoBaixa;
var
  LJsonObject, LJsonCnpjCPF: TACBrJSONObject;
begin
   if Assigned(aTitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    LJsonCnpjCPF := TACBrJSONObject.Create;
    try
      LJsonCnpjCPF.AddPair('cpfCnpj', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
      LJsonCnpjCPF.AddPair('filial', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
      LJsonCnpjCPF.AddPair('controle', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2));
      LJsonObject.AddPair('cpfCnpj', LJsonCnpjCPF);
      LJsonObject.AddPair('produto', RemoveZerosEsquerda(aTitulo.Carteira));
      LJsonObject.AddPair('negociacao', AgenciaContaFormatada(11));
      LJsonObject.AddPair('nossoNumero', OnlyNumber(aTitulo.NossoNumero));
      LJsonObject.AddPair('sequencia', '0');
      LJsonObject.AddPair('codigoBaixa', '57');

      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoJson;
var
  LJsonObject: TACBrJSONObject;
begin
  //*OBS: Todos os campos devem ser informados conforme layout, entretanto, para os tipos não obrigatórios,
  //devem ser preenchidos com zeros para campo numérico, ou espaços para campo alfanumérico.

  if Assigned(aTitulo) then
  begin
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
      LJsonObject.AddPair('cidtfdProdCobr', RemoveZerosEsquerda(aTitulo.Carteira));
      LJsonObject.AddPair('cnegocCobr', AgenciaContaFormatada(18));
      LJsonObject.AddPair('codigoBanco', 237);//FIXO.
      LJsonObject.AddPair('filler', '');//FIXO.
      LJsonObject.AddPair('eNseqContrNegoc', 0);//FIXO.
      //tipoRegistro: 1-Título 2-Título com Instrução de Protesto 3-Título com Instrução de Protesto Falimentar.
      LJsonObject.AddPair('tipoRegistro', 1);//NÃO Obrigatório;
      LJsonObject.AddPair('cprodtServcOper', 0);//FIXO.
      LJsonObject.AddPair('ctitloCobrCdent', OnlyNumber(aTitulo.NossoNumero));//NÃO Obrigatório;

      //ctitloCliCdent: Identificador do título pelo beneficiário(Seu Número).
      LJsonObject.AddPair('ctitloCliCdent', Trim(IfThen(ATitulo.NumeroDocumento <> '',
                                             ATitulo.NumeroDocumento,
                                             IfThen(ATitulo.SeuNumero <> '',
                                                    ATitulo.SeuNumero,
                                                    OnlyNumber(aTitulo.NossoNumero)))));
      LJsonObject.AddPair('demisTitloCobr', DateTimeToDateBradesco(aTitulo.DataDocumento));
      LJsonObject.AddPair('dvctoTitloCobr', DateTimeToDateBradesco(aTitulo.Vencimento));
      LJsonObject.AddPair('cidtfdTpoVcto', 0);//FIXO.
      LJsonObject.AddPair('cindcdEconmMoeda', '00006');
      LJsonObject.AddPair('vnmnalTitloCobr', aTitulo.ValorDocumento*100);
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
      LJsonObject.AddPair('cformaEmisPplta', 2);//FIXO.
      LJsonObject.AddPair('cindcdPgtoParcial', 'N');//FIXO.
      LJsonObject.AddPair('qtdePgtoParcial', 000);//FIXO.
      LJsonObject.AddPair('filler1', '');//FIXO.

      GerarJuros(LJsonObject);
      GerarMulta(LJsonObject);
      GerarDesconto(LJsonObject);

      //Tipo de prazo desconto/bonificação: 1 = Dias corridos | 2 = Dias úteis. Obrigatório? Sim, caso informado valor ou percentual de desconto/bonificação.
      LJsonObject.AddPair('ctpoPrzCobr', 1);
      LJsonObject.AddPair('pdescBonifPgto', 0);//NÃO Obrigatório;
      LJsonObject.AddPair('vdescBonifPgto', 0);//NÃO Obrigatório;
      LJsonObject.AddPair('dlimBonifPgto', '');// 'Exemplo 01.01.2001';Caso informado o acima.
      LJsonObject.AddPair('vabtmtTitloCobr', 0);//NÃO Obrigatório;
      LJsonObject.AddPair('viofPgtoTitlo', 0);//NÃO Obrigatório;
      LJsonObject.AddPair('filler2', '');//FIXO.
      LJsonObject.AddPair('isacdoTitloCobr', Copy(TiraAcentos(aTitulo.Sacado.NomeSacado), 1, 70));
      LJsonObject.AddPair('elogdrSacdoTitlo', Copy(TiraAcentos(aTitulo.Sacado.Logradouro), 1, 40));
      LJsonObject.AddPair('enroLogdrSacdo', aTitulo.Sacado.Numero);
      LJsonObject.AddPair('ecomplLogdrSacdo', Copy(TiraAcentos(aTitulo.Sacado.Complemento), 1, 15));
      LJsonObject.AddPair('ccepSacdoTitlo', Copy(OnlyNumber(aTitulo.Sacado.CEP), 1, 5));
      LJsonObject.AddPair('ccomplCepSacdo', Copy(OnlyNumber(aTitulo.Sacado.CEP), 6, 8));
      LJsonObject.AddPair('ebairoLogdrSacdo', Copy(TiraAcentos(aTitulo.Sacado.Bairro), 1, 40));
      LJsonObject.AddPair('imunSacdoTitlo', Copy(TiraAcentos(aTitulo.Sacado.Cidade), 1, 30));
      LJsonObject.AddPair('csglUfSacdo', Copy(TiraAcentos(aTitulo.Sacado.UF), 1, 2));
      LJsonObject.AddPair('indCpfCnpjSacdo', IfThen(aTitulo.Sacado.Pessoa = pJuridica, '2', '1'));
      LJsonObject.AddPair('nroCpfCnpjSacdo', OnlyNumber(aTitulo.Sacado.CNPJCPF));
      LJsonObject.AddPair('renderEletrSacdo', Copy(aTitulo.Sacado.Email, 1, 70));
      LJsonObject.AddPair('cdddFoneSacdo', Copy(OnlyNumber(aTitulo.Sacado.Fone), 1, 3));//NÃO Obrigatório;
      LJsonObject.AddPair('cfoneSacdoTitlo', Copy(OnlyNumber(aTitulo.Sacado.Fone), 1, 11));//NÃO Obrigatório;
      LJsonObject.AddPair('bancoDeb', 0);//Código do Banco para débito automático. -> NÃO Obrigatório;
      LJsonObject.AddPair('agenciaDeb', 0);//Número da agência para débito automático. -> NÃO Obrigatório;
      LJsonObject.AddPair('agenciaDebDv', 0);//Dígito verificador da Agência para débito automático. -> Caso informado agenciaDeb;
      LJsonObject.AddPair('contaDeb', 0);//Número da conta para débito automático. -> Caso informado agenciaDeb;
      LJsonObject.AddPair('bancoCentProt', 0);//FIXO.
      LJsonObject.AddPair('agenciaDvCentPr', 0);//FIXO.

      GerarBenificiarioFinal(LJsonObject);

      LJsonObject.AddPair('filler3', '');//FIXO.
      LJsonObject.AddPair('fase', 1);//FIXO.
      LJsonObject.AddPair('cindcdCobrMisto', 'S');//FIXO.
      LJsonObject.AddPair('ialiasAdsaoCta', '');//Chave Pix do beneficiário. Manter em branco.
      LJsonObject.AddPair('iconcPgtoSpi', '');//TXID do título. Manter em branco.
      LJsonObject.AddPair('caliasAdsaoCta', '');//Códigos de erro na geração do QR Code pelo BSPI. Manter em branco.
      LJsonObject.AddPair('ilinkGeracQrcd', '');//Identificação do location do QR Code gerado pelo BSPI. Manter em branco.
      LJsonObject.AddPair('wqrcdPdraoMercd', '');//Código EMV do QR Code gerado pelo BSPI. Manter em branco.
      LJsonObject.AddPair('validadeAposVencimento', '');//Quantidade de dias após vencimento, que o título é válido para pagamento via Pix. Manter em branco.
      LJsonObject.AddPair('filler4', '');//Manter em branco.

      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoAltera;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroContrato',aTitulo.ACBrBoleto.Cedente.CodigoCedente);
      LJsonObject.AddPair('modalidade',aTitulo.ACBrBoleto.Cedente.Modalidade);
      LJsonObject.AddPair('nossoNumero',OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)));

      case Integer(aTitulo.ACBrBoleto.ListadeBoletos.Objects[0].OcorrenciaOriginal.Tipo) of
         1 : // Baixa
          begin
            LJsonObject.AddPair('seuNumero', IfThen(ATitulo.SeuNumero <> '',
                                                      ATitulo.SeuNumero,
                                                      IfThen(ATitulo.NumeroDocumento <> '',
                                                        ATitulo.NumeroDocumento,
                                                        OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
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
            GerarJuros(LJsonObject);
          end;
        50:  //RemessaAlterarMulta
          begin
            GerarMulta(LJsonObject);
          end;
        52: //RemessaAlterarDesconto
          begin
            AlteracaoDesconto(LJsonObject);
          end;
        54: //RemessaAlterarAbatimento
          begin
           AtribuirAbatimento(LJsonObject);
          end;
        64:  //Alterar Especie
          begin
            AlterarEspecie(LJsonObject);
          end;
      end;

      FPDadosMsg := Format('[%s]',[LJsonObject.ToJSON]);
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_Bradesco.RequisicaoConsultaDetalhe;
var
  LJsonObject, LJsonCpfCnpj: TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      //Testado em produção
      LJsonCpfCnpj := TACBrJSONObject.Create;
      LJsonCpfCnpj.AddPair('cpfCnpj', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 1, 8));
      LJsonCpfCnpj.AddPair('filial', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 9, 4));
      LJsonCpfCnpj.AddPair('controle', Copy(OnlyNumber(Boleto.Cedente.CNPJCPF), 13, 2));
      LJsonObject.AddPair('cpfCnpj', LJsonCpfCnpj);
      
      LJsonObject.AddPair('produto', RemoveZerosEsquerda(aTitulo.Carteira));
      LJsonObject.AddPair('negociacao', AgenciaContaFormatada(11));
      LJsonObject.AddPair('nossoNumero', OnlyNumber(aTitulo.NossoNumero));
      LJsonObject.AddPair('sequencia', '0');
      LJsonObject.AddPair('status', 0);

      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_Bradesco.GerarBenificiarioFinal(AJsonObject: TACBrJSONObject);
begin
  //*Nenhum desses campos são obrigatórios, mas caso informado "Nome" do avalista, deve informar os demais abaixo.
  if Assigned(aTitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
      AJsonObject.AddPair('isacdrAvalsTitlo', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.NomeAvalista), 1, 40));
      AJsonObject.AddPair('nroCpfCnpjSacdr', Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF), 1, 14));
      AJsonObject.AddPair('ccepSacdrTitlo', Copy(aTitulo.Sacado.SacadoAvalista.CEP, 1, 5));
      AJsonObject.AddPair('ccomplCepSacdr', Copy(aTitulo.Sacado.SacadoAvalista.CEP, 6, 8));
      AJsonObject.AddPair('elogdrSacdrAvals', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Logradouro), 1, 10));
      AJsonObject.AddPair('enroLogdrSacdr', aTitulo.Sacado.SacadoAvalista.Numero);
      AJsonObject.AddPair('ecomplLogdrSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Complemento), 1, 15));
      AJsonObject.AddPair('ebairoLogdrSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Bairro), 1, 40));
      AJsonObject.AddPair('imunSacdrAvals', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.Cidade), 1, 40));
      AJsonObject.AddPair('csglUfSacdr', Copy(TiraAcentos(aTitulo.Sacado.SacadoAvalista.UF), 1, 2));
      AJsonObject.AddPair('indCpfCnpjSacdr', IfThen(aTitulo.Sacado.SacadoAvalista.Pessoa = pJuridica, '2', '1'));
      AJsonObject.AddPair('renderEletrSacdr', Copy(aTitulo.Sacado.SacadoAvalista.Email, 1, 70));
      AJsonObject.AddPair('cdddFoneSacdr', Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.Fone), 1, 3));//NÃO Obrigatório;
      AJsonObject.AddPair('cfoneSacdrTitlo', Copy(OnlyNumber(aTitulo.Sacado.SacadoAvalista.Fone), 1, 11));//NÃO Obrigatório;
    end;
  end;
end;

procedure TBoletoW_Bradesco.GerarJuros(AJsonObject: TACBrJSONObject);
begin
 if Assigned(aTitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
      if ATitulo.CodigoMora = '' then
      begin
        case aTitulo.CodigoMoraJuros of
          cjValorDia   : aTitulo.CodigoMora := '1';
          cjTaxaMensal : aTitulo.CodigoMora := '2';
          cjIsento     : aTitulo.CodigoMora := '3';
          else
            aTitulo.CodigoMora := '3';
        end;
      end;
      //qdiaInicJuro: Quantidade de dias após o vencimento, para incidência de juros
      case (StrToIntDef(aTitulo.CodigoMora, 0)) of
        0,3:    // Isento
          begin
            AJsonObject.AddPair('ptxJuroVcto', 0);
            AJsonObject.AddPair('vdiaJuroMora', 0);
            AJsonObject.AddPair('qdiaInicJuro', 0);
          end;
        1:     // Dia
          begin
            AJsonObject.AddPair('vdiaJuroMora', aTitulo.ValorMoraJuros*100);
            if aTitulo.ValorMoraJuros > 0 then
              AJsonObject.AddPair('qdiaInicJuro', DaysBetween(aTitulo.Vencimento, aTitulo.DataMoraJuros));
            AJsonObject.AddPair('ptxJuroVcto', 0);
          end;
        2: // Mês
          begin
            AJsonObject.AddPair('ptxJuroVcto', aTitulo.ValorMoraJuros*100);
            if aTitulo.ValorMoraJuros > 0 then
              AJsonObject.AddPair('qdiaInicJuro', DaysBetween(aTitulo.Vencimento, aTitulo.DataMoraJuros))
            else
              AJsonObject.AddPair('qdiaInicJuro', 0);
            AJsonObject.AddPair('vdiaJuroMora', 0);
          end;
      end;
    end;
  end;
end;

procedure TBoletoW_Bradesco.GerarMulta(AJsonObject: TACBrJSONObject);
var
  LCodMulta: Integer;
  LDataMulta : TDateTime;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
      if aTitulo.PercentualMulta > 0 then
      begin
        if aTitulo.MultaValorFixo then
          LCodMulta := 1
        else
          LCodMulta := 2;
      end
      else
        LCodMulta := 3;
      if (aTitulo.DataMulta > 0) then
        LDataMulta :=  aTitulo.DataMulta
      else
        LDataMulta  := ATitulo.DataMoraJuros;
      case LCodMulta of
        1:
        begin
          AJsonObject.AddPair('pmultaAplicVcto', 0);
          AJsonObject.AddPair('vmultaAtrsoPgto', aTitulo.PercentualMulta*100);
          AJsonObject.AddPair('qdiaInicMulta', DaysBetween(aTitulo.Vencimento, LDataMulta));
        end;
        2:
        begin
          AJsonObject.AddPair('pmultaAplicVcto', aTitulo.PercentualMulta);
          AJsonObject.AddPair('vmultaAtrsoPgto', 0);
          AJsonObject.AddPair('qdiaInicMulta', DaysBetween(aTitulo.Vencimento, LDataMulta));
        end;
        3:
        begin
          AJsonObject.AddPair('pmultaAplicVcto', 0);
          AJsonObject.AddPair('vmultaAtrsoPgto', 0);
          AJsonObject.AddPair('qdiaInicMulta', 0);
        end;
      end;
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
  result := inherited GerarRemessa;
end;

function TBoletoW_Bradesco.Enviar: boolean;
begin
  DefinirCertificado;
  result := inherited Enviar;
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
begin
 if Assigned(aTitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
      // '1'  =  Valor em Reais;
      // '2'  =  Percentual;
      // '3'  =  Isento.
      if Integer(aTitulo.TipoDesconto) <> 0 then
      begin
        if Integer(aTitulo.TipoDesconto) = 1 then
        begin
          AJsonObject.AddPair('pdescBonifPgto01', 0);
          AJsonObject.AddPair('vdescBonifPgto01', aTitulo.ValorDesconto);
        end
        else
        begin
          AJsonObject.AddPair('pdescBonifPgto01', aTitulo.ValorDesconto);
          AJsonObject.AddPair('vdescBonifPgto01', 0);
        end;
        AJsonObject.AddPair('dlimDescBonif1', DateTimeToDateBradesco(aTitulo.Vencimento));
      end
      else
      begin
        AJsonObject.AddPair('pdescBonifPgto01', 0);
        AJsonObject.AddPair('vdescBonifPgto01', 0);
        AJsonObject.AddPair('dlimDescBonif1', '');
      end;
      if Integer(aTitulo.TipoDesconto2) <> 0 then
      begin
        if Integer(aTitulo.TipoDesconto2) = 1 then
        begin
          AJsonObject.AddPair('pdescBonifPgto02', 0);
          AJsonObject.AddPair('vdescBonifPgto02', aTitulo.ValorDesconto);
        end
        else
        begin
          AJsonObject.AddPair('pdescBonifPgto02', aTitulo.ValorDesconto);
          AJsonObject.AddPair('vdescBonifPgto02', 0);
        end;
        AJsonObject.AddPair('dlimDescBonif2', DateTimeToDateBradesco(aTitulo.Vencimento));
      end
      else
      begin
        AJsonObject.AddPair('pdescBonifPgto02', 0);
        AJsonObject.AddPair('vdescBonifPgto02', 0);
        AJsonObject.AddPair('dlimDescBonif2', '');
      end;
      if Integer(aTitulo.TipoDesconto3) <> 0 then
      begin
        if Integer(aTitulo.TipoDesconto3) = 1 then
        begin
          AJsonObject.AddPair('pdescBonifPgto03', 0);
          AJsonObject.AddPair('vdescBonifPgto03', aTitulo.ValorDesconto);
        end
        else
        begin
          AJsonObject.AddPair('pdescBonifPgto03', aTitulo.ValorDesconto);
          AJsonObject.AddPair('vdescBonifPgto03', 0);
        end;
        AJsonObject.AddPair('dlimDescBonif3', DateTimeToDateBradesco(aTitulo.Vencimento));
      end
      else
      begin
        AJsonObject.AddPair('pdescBonifPgto03', 0);
        AJsonObject.AddPair('vdescBonifPgto03', 0);
        AJsonObject.AddPair('dlimDescBonif3', '');
      end;
    end;
  end;
end;

procedure TBoletoW_Bradesco.AlteraDataVencimento(AJsonObject: TACBrJSONObject);
begin
 if Assigned(ATitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
        if (ATitulo.Vencimento > 0) then
        begin
          AJsonObject.AddPair('dataVencimento',DateTimeToDateBradesco(aTitulo.Vencimento));
        end;
    end;
  end;
end;

procedure TBoletoW_Bradesco.AtribuirAbatimento(AJsonObject: TACBrJSONObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
        if (ATitulo.Vencimento > 0) then
        begin
          AJsonObject.AddPair('valorAbatimento',aTitulo.ValorAbatimento);
        end;
    end;
  end;
end;

procedure TBoletoW_Bradesco.AlterarEspecie(AJsonObject: TACBrJSONObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
        if (ATitulo.Vencimento > 0) then
        begin
          AJsonObject.AddPair('especieDocumento',EspecieDocumento);
        end;
    end;
  end;
end;

procedure TBoletoW_Bradesco.AtribuirDesconto(AJsonObject: TACBrJSONObject);
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
      GerarDesconto(AJsonObject);
    end;
  end;
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
 if Assigned(ATitulo) then
  begin
    if Assigned(AJsonObject) then
    begin
       GerarDesconto(AJsonObject);
    end;
  end;
end;

procedure TBoletoW_Bradesco.AlterarProtesto(AJsonObject: TACBrJSONObject);
begin
  // Só Precisa de Numero de Contrato, Modalidade e Nosso Numero

  // Já preenchidos
end;

end.

