{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Marcelo Santos,  }
{ Delmar de Lima                                                               }
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

unit ACBrBoletoW_Bancoob;

interface

uses

  ACBrJSON,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest,
  DateUtils,
  ACBrDFeSSL,
  synautil,
  httpsend,
  Math;


type

  { TBoletoW_Bancoob}
  TBoletoW_Bancoob = class(TBoletoWSREST)
  private
    function DateBancoobtoDateTime(const AValue: String): TDateTime;
    function DateTimeToDateBancoob( const AValue:TDateTime ):String;
    procedure GerarInstrucao(AJson: TACBrJSONObject);
    procedure AlterarEspecie(AJson: TACBrJSONObject);
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
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsultaDetalhe;
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure AlteraDataVencimento(AJson: TACBrJSONObject);
    procedure AtribuirDesconto(AJson: TACBrJSONObject);
    procedure AlteracaoDesconto(AJson: TACBrJSONObject);
    procedure AlterarProtesto(AJson: TACBrJSONObject);
    procedure AtribuirAbatimento(AJson: TACBrJSONObject);
    procedure AtribuirJuros(AJson: TACBrJSONObject);
    procedure AtribuirMulta(AJson: TACBrJSONObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;
  end;

const
  C_URL             = 'https://api.sicoob.com.br/cobranca-bancaria/v2';
  C_URL_HOM         = 'https://sandbox.sicoob.com.br/sicoob/sandbox/cobranca-bancaria/v2';

  C_URL_OAUTH_PROD  = 'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';
  C_URL_OAUTH_HOM   = 'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';

  C_CONTENT_TYPE    = 'application/json';
  C_ACCEPT          = 'application/json';
  C_AUTHORIZATION   = 'Authorization';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'utf-8';
  C_SICOOB_CLIENT   = 'client_id';

  C_ACCESS_TOKEN_HOM = '1301865f-c6bc-38f3-9f49-666dbcfc59c3';

implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Base,
  SysUtils,
  Classes,
  Synacode,
  StrUtils,

  pcnConversao,
  ACBrBoletoWS.Rest.OAuth,
  ACBrBoletoConversao;

{ TBoletoW_Bancoob}

procedure TBoletoW_Bancoob.DefinirURL;
var
  LNossoNumero, LContrato: string;
begin

  if( aTitulo <> nil ) then
  begin
    LNossoNumero := ACBrUtil.Strings.RemoveZerosEsquerda(OnlyNumber(aTitulo.NossoNumero)+aTitulo.ACBrBoleto.Banco.CalcularDigitoVerificador(aTitulo));
    LContrato    := OnlyNumber(aTitulo.ACBrBoleto.Cedente.CodigoCedente);
  end;
  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL,C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:  FPURL := FPURL + '/boletos';
    tpAltera:
    begin
       if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaBaixar then
         FPURL := FPURL + '/boletos/baixa'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaConcederDesconto then
         FPURL := FPURL + '/boletos/descontos'
       else if aTitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarVencimento then
         FPURL := FPURL + '/boletos/prorrogacoes/data-vencimento'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaProtestar then
         FPURL := FPURL + '/boletos/protestos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaSustarProtesto then
         FPURL := FPURL + '/boletos/protestos'
       else if ATitulo.OcorrenciaOriginal.Tipo in [ACBrBoleto.toRemessaCobrarJurosMora, ACBrBoleto.toRemessaAlterarJurosMora] then
         FPURL := FPURL + '/boletos/encargos/juros-mora'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarMulta then
         FPURL := FPURL + '/boletos/encargos/multas'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarDesconto then
         FPURL := FPURL +  '/boletos/descontos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarValorAbatimento then
         FPURL := FPURL + '/boletos/abatimentos'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarSeuNumero then
         FPURL := FPURL + '/boletos/seu-numero'
       else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarEspecieTitulo then
         FPURL := FPURL + '/boletos/especie-documento';
    end;
    tpConsultaDetalhe:  FPURL := FPURL + '/boletos?numeroContrato='+LContrato+'&modalidade=1&nossoNumero='+LNossoNumero;
    tpBaixa:  FPURL := FPURL + '/boletos/baixa';
  end;

end;

procedure TBoletoW_Bancoob.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Bancoob.GerarHeader;
begin
  FPHeaders.Clear;
  DefinirContentType;
  DefinirKeyUser;

  if NaoEstaVazio(Boleto.Cedente.CedenteWS.ClientID) then
    FPHeaders.Add(C_SICOOB_CLIENT + ': ' + Boleto.Cedente.CedenteWS.ClientID);
//  HTTPSend.Headers.Add('Accept-Encoding: ' + C_ACCEPT_ENCODING);
end;

procedure TBoletoW_Bancoob.GerarDados;
begin
  if Assigned(Boleto) then

    DefinirURL;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        FMetodoHTTP := htPOST; // Define Método POST para Incluir
        RequisicaoJson;
      end;
    tpAltera:
      begin
        FMetodoHTTP := htPATCH;
        RequisicaoAltera;
      end;
    tpBaixa:
      begin
        FMetodoHTTP := htPATCH; // Define Método POST para Baixa
        RequisicaoBaixa;
      end;
    tpConsultaDetalhe:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta Detalhe
        RequisicaoConsultaDetalhe;
      end;

  else
    raise EACBrBoletoWSException.Create
      (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;
end;

procedure TBoletoW_Bancoob.DefinirAuthorization;
begin
  if Boleto.Configuracoes.WebService.Ambiente = taProducao then
    FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao
  else
    FPAuthorization := C_AUTHORIZATION + ': Bearer ' + C_ACCESS_TOKEN_HOM;
end;

function TBoletoW_Bancoob.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Bancoob.DefinirKeyUser;
begin
  FPKeyUser := '';
end;

function TBoletoW_Bancoob.DefinirParametros: String;
var
  Consulta: TStringList;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    Consulta := TStringList.Create;
    Consulta.Delimiter := '&';
    try
      Consulta.Add( 'numeroContrato='+Boleto.Cedente.CodigoCedente);
      Consulta.Add( 'modalidade=1' );
      // Consulta.Add( 'nossoNumero=124' );
    finally
      result := Consulta.DelimitedText;
      Consulta.Free;
    end;
  end;
end;

procedure TBoletoW_Bancoob.DefinirParamOAuth;
begin
  FParamsOAuth := Format( 'client_id=%s&scope=%s&grant_type=client_credentials',
                   [Boleto.Cedente.CedenteWS.ClientID,
                    Boleto.Cedente.CedenteWS.Scope] );
end;

function TBoletoW_Bancoob.DateBancoobtoDateTime(const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0); 
end;

function TBoletoW_Bancoob.DateTimeToDateBancoob(const AValue: TDateTime): String;
begin
  //result := DateTimeToIso8601(DateTimeUniversal('',AValue),BiasToTimeZone(LTZ.Bias));
  result := FormatDateBr( aValue, 'YYYY-MM-DD') + 'T' + FormatDateTime('hh:nn:ss', AValue) + GetUTCSistema;
end;

procedure TBoletoW_Bancoob.DefinirAutenticacao;
begin

end;

function TBoletoW_Bancoob.ValidaAmbiente: Integer;
begin
  result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'), 2);
end;

procedure TBoletoW_Bancoob.RequisicaoBaixa;
var
  LJson: TACBrJSONObject;
  LData: string;
begin
  if not Assigned(aTitulo) then
    Exit;

  LJson := TACBrJSONObject.Create;
  try
    LJson.AddPair('numeroContrato',StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0));
    LJson.AddPair('modalidade',StrToIntDef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1));
    LJson.AddPair('nossoNumero',StrToIntDef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0));
    LJson.AddPair('seuNumero',IfThen(ATitulo.SeuNumero <> '',
                                                    ATitulo.SeuNumero,
                                                    IfThen(ATitulo.NumeroDocumento <> '',
                                                      ATitulo.NumeroDocumento,
                                                      OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                    )
                                                  ));

    FPDadosMsg := '[' + LJson.ToJSON + ']';
  finally
    LJson.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoJson;
var
  Data: string;
  LJson: TACBrJSONObject;
begin
  if not Assigned(aTitulo) then
    Exit;

  LJson := TACBrJSONObject.Create;
  try
    LJson.AddPair('numeroContrato',StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0));
    LJson.AddPair('modalidade',StrToIntdef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1));
    LJson.AddPair('numeroContaCorrente',StrToInt64(aTitulo.ACBrBoleto.Cedente.Conta + aTitulo.ACBrBoleto.Cedente.ContaDigito));
    LJson.AddPair('especieDocumento',aTitulo.EspecieDoc);
    LJson.AddPair('dataEmissao',DateTimeToDateBancoob(aTitulo.DataDocumento));
    {
      Número que identifica o boleto de cobrança no Sisbr.
      Caso deseje, o beneficiário poderá informar o nossoNumero,
      Caso contrário, o sistema gerará automáticamente.
    }
    if StrToInt(ATitulo.NossoNumero) > 0 then
      LJson.AddPair('nossoNumero',StrToIntDef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0));

    LJson.AddPair('seuNumero',IfThen(ATitulo.NumeroDocumento <> '',
                                                                     ATitulo.NumeroDocumento,
                                                                     IfThen(ATitulo.SeuNumero <> '',
                                                                       ATitulo.SeuNumero,
                                                                       OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                                     )
                                                                   ));
    LJson.AddPair('identificacaoBoletoEmpresa',IfThen(ATitulo.SeuNumero <> '',
                                                                     ATitulo.SeuNumero,
                                                                     OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo))
                                                                   ));
    LJson.AddPair('identificacaoEmissaoBoleto',StrToInt(IfThen(ATitulo.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite,'1','2'))); // 2 Cliente Emite - 1 Banco Emite
    LJson.AddPair('identificacaoDistribuicaoBoleto',StrToInt(IfThen(ATitulo.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite,'1','2'))); // 2 Cliente Dist - 1 Banco Dist

    LJson.AddPair('valor',aTitulo.ValorDocumento);
    LJson.AddPair('dataVencimento',DateTimeToDateBancoob(aTitulo.Vencimento));
    LJson.AddPair('numeroParcela',max(1,ATitulo.Parcela));
    LJson.AddPair('aceite',ATitulo.Aceite = atSim);

    if (ATitulo.DataProtesto > 0) then
    begin
      LJson.AddPair('codigoProtesto',IfThen(ATitulo.TipoDiasProtesto = diCorridos, 1, 2));
      LJson.AddPair('numeroDiasProtesto',Trunc(ATitulo.DataProtesto - ATitulo.Vencimento));
    end;
    if (ATitulo.DiasDeNegativacao > 0) then
    begin
      LJson.AddPair('codigoNegativacao',2);
      LJson.AddPair('numeroDiasNegativacao',ATitulo.DiasDeNegativacao);
    end;

    GerarDesconto(LJson);
    GerarJuros(LJson);
    GerarMulta(LJson);
    GerarPagador(LJson);
    GerarBenificiarioFinal(LJson);
    GerarInstrucao(LJson);

    LJson.AddPair('gerarPdf',false);
    LJson.AddPair('codigoCadastrarPIX',StrToInt(IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'1','0')));

    FPDadosMsg := '['+LJson.ToJSON+']';
  finally
    LJson.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoAltera;
var
  LData: string;
  LJson: TACBrJSONObject;
  LNumeroDocumento: string;
begin
  if not Assigned(aTitulo) then
    Exit;

  LJson := TACBrJSONObject.Create;
  try
    LJson.AddPair('numeroContrato',StrToIntDef(aTitulo.ACBrBoleto.Cedente.CodigoCedente, 0));
    LJson.AddPair('modalidade',strtoIntdef(aTitulo.ACBrBoleto.Cedente.Modalidade, 1));
    LJson.AddPair('nossoNumero',StrtoIntdef(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 0));

    LNumeroDocumento := Trim(ATitulo.SeuNumero);
    if EstaVazio(LNumeroDocumento) then
      LNumeroDocumento := Trim(ATitulo.NumeroDocumento);
    if EstaVazio(LNumeroDocumento) then
      LNumeroDocumento := OnlyNumber(ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));

    case aTitulo.ACBrBoleto.ListadeBoletos.Objects[0].OcorrenciaOriginal.Tipo of
      toRemessaBaixar, toRemessaAlterarSeuNumero:
        LJson.AddPair('seuNumero',LNumeroDocumento);
      toRemessaConcederDesconto:
        AtribuirDesconto(LJson);
      toRemessaAlterarVencimento:
        AlteraDataVencimento(LJson);
      toRemessaProtestar: begin
        FMetodoHTTP := htPOST;
        AlterarProtesto(LJson);
      end;
      toRemessaSustarProtesto: begin
        FMetodoHTTP :=  htDELETE;
        AlterarProtesto(LJson);
      end;
      toRemessaAlterarJurosMora, toRemessaCobrarJurosMora:
        AtribuirJuros(LJson);
      toRemessaAlterarMulta:
        AtribuirMulta(LJson);
      toRemessaAlterarDesconto:
        AlteracaoDesconto(LJson);
      toRemessaAlterarValorAbatimento:
        AtribuirAbatimento(LJson);
      toRemessaAlterarEspecieTitulo:
        AlterarEspecie(LJson);
    end;

    FPDadosMsg := '['+LJson.ToJSON+']';
  finally
    LJson.Free;
  end;
end;

procedure TBoletoW_Bancoob.RequisicaoConsultaDetalhe;
begin
  FPDadosMsg := '';
end;

procedure TBoletoW_Bancoob.GerarPagador(AJson: TACBrJSONObject);
 var
  LJsonDadosPagador: TACBrJSONObject;
  LJsonArrayEmail: TACBrJSONArray;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  LJsonDadosPagador := TACBrJSONObject.Create;
  LJsonDadosPagador.AddPair('numeroCpfCnpj',OnlyNumber(aTitulo.Sacado.CNPJCPF));
  LJsonDadosPagador.AddPair('nome',aTitulo.Sacado.NomeSacado);
  LJsonDadosPagador.AddPair('endereco',aTitulo.Sacado.Logradouro + ' ' + aTitulo.Sacado.Numero);
  LJsonDadosPagador.AddPair('bairro',aTitulo.Sacado.Bairro);
  LJsonDadosPagador.AddPair('cidade',aTitulo.Sacado.Cidade);
  LJsonDadosPagador.AddPair('cep',OnlyNumber(aTitulo.Sacado.CEP));
  LJsonDadosPagador.AddPair('uf',aTitulo.Sacado.UF);
  if NaoEstaVazio(ATitulo.Sacado.Email) then
  begin
    LJsonArrayEmail := TACBrJSONArray.Create;
    LJsonArrayEmail.AddElement(ATitulo.Sacado.Email);
    LJsonDadosPagador.AddPair('email', LJsonArrayEmail);
  end;
  AJson.AddPair('pagador', LJsonDadosPagador);
end;

procedure TBoletoW_Bancoob.GerarInstrucao(AJson: TACBrJSONObject);
var
  JsonPairInstrucao, JsonDadosInstrucao: TACBrJSONObject;
  JsonArrayInstrucao: TACBrJSONArray;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if ATitulo.Instrucao1 = '' then
    Exit;

  JsonDadosInstrucao := TACBrJSONObject.Create;
  JsonArrayInstrucao := TACBrJSONArray.Create;
  JsonDadosInstrucao.AddPair('tipoInstrucao',1);
  if NaoEstaVazio(ATitulo.Instrucao1) then
    JsonArrayInstrucao.AddElement(ATitulo.Instrucao1);
  if NaoEstaVazio(ATitulo.Instrucao2) then
    JsonArrayInstrucao.AddElement(ATitulo.Instrucao2);
  if NaoEstaVazio(ATitulo.Instrucao3) then
    JsonArrayInstrucao.AddElement(ATitulo.Instrucao3);
  JsonDadosInstrucao.AddPair('mensagens', JsonArrayInstrucao);
  AJson.AddPair('mensagensInstrucao', JsonDadosInstrucao);
end;

procedure TBoletoW_Bancoob.GerarBenificiarioFinal(AJson: TACBrJSONObject);
var
 LJsonSacadorAvalista: TACBrJSONObject;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  if aTitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
    Exit;

  LJsonSacadorAvalista := TACBrJSONObject.Create;
  LJsonSacadorAvalista.AddPair('nome',aTitulo.Sacado.SacadoAvalista.NomeAvalista);
  LJsonSacadorAvalista.AddPair('cpfCnpj',OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF));
  LJsonSacadorAvalista.AddPair('tipoPessoa',IfThen(Length(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, 'FISICA', 'JURIDICA'));
  LJsonSacadorAvalista.AddPair('cep',aTitulo.Sacado.SacadoAvalista.CEP);
  LJsonSacadorAvalista.AddPair('endereco',aTitulo.Sacado.SacadoAvalista.Logradouro);
  LJsonSacadorAvalista.AddPair('bairro',aTitulo.Sacado.SacadoAvalista.Bairro);
  LJsonSacadorAvalista.AddPair('cidade',aTitulo.Sacado.SacadoAvalista.Cidade);
  LJsonSacadorAvalista.AddPair('uf',aTitulo.Sacado.SacadoAvalista.UF);
  AJson.AddPair('beneficiarioFinal', LJsonSacadorAvalista);
end;

procedure TBoletoW_Bancoob.GerarJuros(AJson: TACBrJSONObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

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

  case (StrToIntDef(aTitulo.CodigoMora, 0)) of
    0, 3:    // Isento
      begin
        AJson.AddPair('tipoJurosMora',3);
        AJson.AddPair('valorJurosMora',0);
      end;
    1:     // Dia
      begin
       // AJson.AddPair('taxa').Value.asNumber := aTitulo.ValorMoraJuros;
        AJson.AddPair('tipoJurosMora',StrToInt(aTitulo.CodigoMora));
        AJson.AddPair('dataJurosMora',DateTimeToDateBancoob(aTitulo.DataMoraJuros));
        AJson.AddPair('valorJurosMora',aTitulo.ValorMoraJuros);
      end;
    2: // Mês
      begin
        AJson.AddPair('tipoJurosMora',StrToInt(aTitulo.CodigoMora));
        AJson.AddPair('dataJurosMora',DateTimeToDateBancoob(aTitulo.DataMoraJuros));
        AJson.AddPair('valorJurosMora',aTitulo.ValorMoraJuros);
      end;
  end;
end;

procedure TBoletoW_Bancoob.GerarMulta(AJson: TACBrJSONObject);
var
  LCodMulta: Integer;
  LDataMulta : TDateTime;
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

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
        AJson.AddPair('tipoMulta',1); // Valor Fixo
        AJson.AddPair('dataMulta',DateTimeToDateBancoob(LDataMulta));
        AJson.AddPair('valorMulta',aTitulo.PercentualMulta);
      end;
    2:
      begin
        AJson.AddPair('tipoMulta',2); // Percentual
        AJson.AddPair('dataMulta',DateTimeToDateBancoob(LDataMulta));
        AJson.AddPair('valorMulta',aTitulo.PercentualMulta);
      end;
    3:
      begin
        AJson.AddPair('tipoMulta',0);
        AJson.AddPair('valorMulta',0);
      end;
  end;
end;

procedure TBoletoW_Bancoob.GerarDesconto(AJson: TACBrJSONObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  // '0'  =  Não Conceder desconto
  // '1'  =  Valor Fixo Até a Data Informada
  // '2'  =  Percentual Até a Data Informada"

  if (aTitulo.DataDesconto > 0) then
  begin
    case Integer(aTitulo.TipoDesconto) of
      1:
        begin
          AJson.AddPair('tipoDesconto',1);
          AJson.AddPair('dataPrimeiroDesconto',DateTimeToDateBancoob(aTitulo.DataDesconto));
          AJson.AddPair('valorPrimeiroDesconto',aTitulo.ValorDesconto);
        end;
      2:
        begin
          AJson.AddPair('tipoDesconto',2);
          AJson.AddPair('dataPrimeiroDesconto',DateTimeToDateBancoob(aTitulo.DataDesconto));
          AJson.AddPair('valorPrimeiroDesconto',aTitulo.ValorDesconto);
        end;
    end;
  end
  else
    AJson.AddPair('tipoDesconto',0);

  if (aTitulo.DataDesconto2 > 0) then
  begin
    AJson.AddPair('dataSegundoDesconto',DateTimeToDateBancoob(aTitulo.DataDesconto2));
    AJson.AddPair('valorSegundoDesconto',aTitulo.ValorDesconto2);
  end;

  if (aTitulo.DataDesconto3 > 0) then
  begin
    AJson.AddPair('dataTerceiroDesconto',DateTimeToDateBancoob(aTitulo.DataDesconto3));
    AJson.AddPair('valorTerceiroDesconto',aTitulo.ValorDesconto3);
  end;
end;

procedure TBoletoW_Bancoob.AlteraDataVencimento(AJson: TACBrJSONObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.Vencimento = 0) then
    Exit;

  AJson.AddPair('dataVencimento',DateTimeToDateBancoob(aTitulo.Vencimento));
end;

procedure TBoletoW_Bancoob.AtribuirAbatimento(AJson: TACBrJSONObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.ValorAbatimento = 0) then
    Exit;

  AJson.AddPair('valorAbatimento',aTitulo.ValorAbatimento);
end;

procedure TBoletoW_Bancoob.AlterarEspecie(AJson: TACBrJSONObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  if (ATitulo.EspecieDoc = '') then
    Exit;

  AJson.Addpair('especieDocumento',aTitulo.EspecieDoc);
end;

procedure TBoletoW_Bancoob.AtribuirDesconto(AJson: TACBrJSONObject);
begin
  if not Assigned(aTitulo) or not Assigned(AJson) then
    Exit;

  GerarDesconto(AJson);
end;

procedure TBoletoW_Bancoob.AlteracaoDesconto(AJson: TACBrJSONObject);
begin
  if not Assigned(ATitulo) or not Assigned(AJson) then
    Exit;

  GerarDesconto(AJson);
end;

procedure TBoletoW_Bancoob.AlterarProtesto(AJson: TACBrJSONObject);
begin
  // Só Precisa de Numero de Contrato, Modalidade e Nosso Numero

  // Já preenchidos
end;

procedure TBoletoW_Bancoob.AtribuirJuros(AJson: TACBrJSONObject);
begin
  GerarJuros(AJson);
end;

procedure TBoletoW_Bancoob.AtribuirMulta(AJson: TACBrJSONObject);
begin
  GerarMulta(AJson);
end;

constructor TBoletoW_Bancoob.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    OAuth.URL := C_URL_OAUTH_PROD;
    OAuth.Payload := True;
  end;
end;

function TBoletoW_Bancoob.GerarRemessa: string;
begin
  DefinirCertificado;
  result := inherited GerarRemessa;
end;

function TBoletoW_Bancoob.Enviar: boolean;
begin
  DefinirCertificado;
  result := inherited Enviar;
end;

end.

