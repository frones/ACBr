{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{ Biblioteca multiplataforma de componentes Delphi para interação com equipa-   }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales           }
{                                                                               }
{ Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr      }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la   }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM     }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto  }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/lgpl-license.php                           }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{ Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170                }
{ ******************************************************************************}

{$I ACBr.inc}
unit ACBrBoletoW_Itau_API;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  ACBrBoleto,
  synautil,
  Jsons,
  ACBrDFeSSL,
  ACBrUtil.Base,
  ACBrBoletoWS.Rest;

type

  { TBoletoW_Itau_API }
  TBoletoW_Itau_API = class(TBoletoWSREST)
  private
    FDFeSSL: TDFeSSL;
    function CodigoTipoTitulo(AEspecieDoc: String): Integer;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;

    procedure RequisicaoJson;
    procedure GerarData(AJson: TJsonObject);
    procedure GeraIdBeneficiario(AJson: TJsonObject);
    procedure GerarDadosQrCode(AJson: TJsonObject);
    procedure GeraDadoBoleto(AJson: TJsonObject);
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarPessoaPg(AJson: TJsonObject);
    procedure GerarTipoPessoaPg(AJson: TJsonObject);
    procedure GerarEnderecoPg(AJson: TJsonObject);
    procedure GerarDadosIndividuaisBoleto(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GerarRecebimentoDivergente(AJson: TJsonObject);
    procedure GerarInstruCaoCobranca(AJson: TJsonObject);
    procedure GerarProtesto(AJson: TJsonObject);
    procedure GerarNegativacao(AJson: TJsonObject);

    procedure RequisicaoAltera;
    procedure AtribuirDesconto(AJson: TJsonObject);
    procedure AtribuirDesconto1(AJson: TJsonObject);
    procedure AlterarProtesto(AJson: TJsonObject);
    procedure AtribuirJuros(AJson: TJsonObject);
    procedure AtribuirMulta(AJson: TJsonObject);

    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure RequisicaoBaixa;

    function CodigoEspeciaDoc: string;
    function TipoDescontoToString(const AValue: TACBrTipoDesconto): string;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros: String;
    function DefinirInstrucaoAlteracao: string;
    function ValidaAmbiente: Integer;
    function CurrToStrItau(const AValue: Real): String;
    function PercToStrItau(const AValue: Real): String;
    function GerarUUID: string;

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const

  C_URL_PIX     = 'https://secure.api.itau/pix_recebimentos_conciliacoes/v2';
  C_URL_PIX_HOM = 'https://sandbox.devportal.itau.com.br/itau-ep9-gtw-pix-recebimentos-conciliacoes-v2-ext/v2';

  C_URL =     'https://api.itau.com.br/cash_management/v2';
  C_URL_HOM = 'https://sandbox.devportal.itau.com.br/itau-ep9-gtw-cash-management-ext-v2/v2';

  C_URL_CONSULTA = 'https://secure.api.cloud.itau.com.br/boletoscash/v2';

  C_URL_OAUTH_PROD = 'https://sts.itau.com.br/api/oauth/token';

  C_URL_OAUTH_HOM = 'https://devportal.itau.com.br/api/jwt';

  C_ACCEPT_PIX = 'application/json';
  C_ACCEPT     = '';

  C_AUTHORIZATION = 'Authorization';

implementation

uses
  synacode, strutils, DateUtils, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TBoletoW_Itau_API }

procedure TBoletoW_Itau_API.DefinirURL;
begin

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM);
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        if Boleto.Cedente.CedenteWS.IndicadorPix then
         FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL_PIX, C_URL_PIX_HOM) + '/boletos_pix'
        else
         FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM) + '/boletos';
      end;

    tpConsulta:
      FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL_CONSULTA, C_URL_HOM) + '/boletos?' + DefinirParametros;

    tpConsultaDetalhe:
      FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,
        C_URL_CONSULTA, C_URL_HOM) + '/boletos?' + DefinirParametros;

    tpAltera:
      FPURL := FPURL + '/boletos/' + DefinirParametros;

    tpBaixa:
      FPURL := FPURL + '/boletos/' + DefinirParametros;
  end;
end;

procedure TBoletoW_Itau_API.DefinirContentType;
begin
  FPContentType := 'application/json';
end;

function TBoletoW_Itau_API.DefinirInstrucaoAlteracao: string;
begin
  if Assigned(Boleto) then
    case Boleto.Configuracoes.WebService.Operacao of
      tpAltera:
        begin
          case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
            3, 4:     Result := 'abatimento';
            5,52,53:  Result := 'desconto';
            7:        Result := 'data_vencimento';
            9,10,12:  Result := 'protesto';
            18:       Result := 'seu_numero';
            37:       Result := 'juros';
            50,51:    Result := 'multa';
            55:       Result := 'data_limite_pagamento' ;
          end;
        end;
      tpBaixa:
        Result := 'baixa';
    else
      Result := '';
    end;
end;

procedure TBoletoW_Itau_API.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_Itau_API.GerarDados;
begin
  if Assigned(Boleto) then
    case Boleto.Configuracoes.WebService.Operacao of
      tpInclui:
        begin
          FMetodoHTTP := htPOST; // Define Método POST para Incluir
          RequisicaoJson;
        end;
      tpAltera:
        begin
          FMetodoHTTP := htPATCH; // Define Método PATCH para alteracao
          RequisicaoAltera;
        end;
      tpBaixa:
        begin
          FMetodoHTTP := htPATCH; // Define Método POST para Baixa
          RequisicaoBaixa;
        end;
      tpConsulta:
        begin
          FMetodoHTTP := htGET; // Define Método GET Consulta
          RequisicaoConsulta;
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

procedure TBoletoW_Itau_API.DefinirAuthorization;
begin
  FPAuthorization := C_Authorization + ': ' + 'Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_Itau_API.GerarTokenAutenticacao: string;
begin
  Result := '';
  if Assigned(OAuth) then
  begin
    OAuth.GrantType := 'client_credentials';
    OAuth.ParamsOAuth := 'grant_type=client_credentials&client_id=' +
      Boleto.Cedente.CedenteWS.ClientID + '&client_secret=' +
      Boleto.Cedente.CedenteWS.ClientSecret;

    OAuth.ContentType := 'application/x-www-form-urlencoded';
    if OAuth.GerarToken then
      Result := OAuth.Token
    else
      raise EACBrBoletoWSException.Create
        (ClassName + Format(S_ERRO_GERAR_TOKEN_AUTENTICACAO,
        [OAuth.ErroComunicacao]));
  end;
end;

function TBoletoW_Itau_API.PercToStrItau(const AValue: Real): String;
begin
 result := StringReplace(FormatFloat('0.00000', AValue), ',', '.', [rfReplaceAll])
end;

procedure TBoletoW_Itau_API.DefinirKeyUser;
begin
  if Boleto.Cedente.CedenteWS.IndicadorPix and Assigned(ATitulo) then
      FPKeyUser := 'x-itau-correlationID: ' + Boleto.Cedente.CedenteWS.ClientID
  else
  begin
    FPHeaders.Add('x-itau-apikey: ' + Boleto.Cedente.CedenteWS.ClientID);
    FPHeaders.Add('x-itau-correlationID: ' + Boleto.Cedente.CedenteWS.ClientID);
  end;
end;

function TBoletoW_Itau_API.DefinirParametros: String;
var
  LNossoNumero, LId_Beneficiario, LCarteira, Documento, LDAC: String;
  LConsulta: TStringList;
begin
  if Assigned(ATitulo) then
    LNossoNumero := ATitulo.NossoNumero;

  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin

    if (Boleto.Cedente.Agencia = EmptyStr) then
      raise EACBrBoletoWSException.Create
        (ClassName + ' Obrigatório informar o agenciaBeneficiario. ');

    if (Boleto.Cedente.Conta = EmptyStr) then
      raise EACBrBoletoWSException.Create
        (ClassName + ' Obrigatório informar o contaBeneficiario. ');

    LDAC := IfThen(Boleto.Cedente.DigitoVerificadorAgenciaConta <> '',
                     Boleto.Cedente.DigitoVerificadorAgenciaConta,
                     Boleto.Cedente.ContaDigito);

    if (LDAC = EmptyStr) then
      raise EACBrBoletoWSException.Create
        (ClassName +
        ' Obrigatório informar o DigitoVerificadorAgenciaContaBeneficiario. ');

    LId_Beneficiario := PadLeft(Boleto.Cedente.Agencia, 4, '0') +
                        PadLeft(Boleto.Cedente.Conta, 7, '0') +
                        PadLeft(LDAC, 1, '0');

    LConsulta := TStringList.Create;
    LConsulta.Delimiter := '&';
    try
      case Boleto.Configuracoes.WebService.Operacao of
        tpConsulta,tpConsultaDetalhe :
          begin
            LConsulta.Add('id_beneficiario=' + LId_Beneficiario);

            LConsulta.Add('codigo_carteira=' + ATitulo.Carteira);

            if LNossoNumero <> EmptyStr then
               LConsulta.Add('nosso_numero=' + LNossoNumero);
          end;
        tpAltera :
          begin
             LConsulta.Add(LId_Beneficiario+
                          ATitulo.Carteira+
                          LNossoNumero+'/'+DefinirInstrucaoAlteracao );
          end;
        tpBaixa :
          begin
             LConsulta.Add(LId_Beneficiario+
                          inttostr(Boleto.Configuracoes.WebService.Filtro.carteira)+
                          LNossoNumero+'/baixa');
          end;
      end;
    finally
      Result := LConsulta.DelimitedText;
      LConsulta.Free;
    end;
  end;
end;

procedure TBoletoW_Itau_API.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Itau_API.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1', '2'), 2);
end;

procedure TBoletoW_Itau_API.GeraIdBeneficiario(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
  LId_Beneficiario, LDAC: string;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      LDAC := IfThen(Boleto.Cedente.DigitoVerificadorAgenciaConta <> '',
                     Boleto.Cedente.DigitoVerificadorAgenciaConta,
                     Boleto.Cedente.ContaDigito);
      try
        LId_Beneficiario := PadLeft(Boleto.Cedente.Agencia, 4, '0') +
          PadLeft(Boleto.Cedente.Conta, 7, '0') +
          PadLeft(LDAC, 1, '0');

        LJsonDados.Add('id_beneficiario').Value.AsString := LId_Beneficiario;

        LJsonPair := TJsonPair.Create(AJson, 'beneficiario');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('beneficiario').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarDadosIndividuaisBoleto(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
  LJsonArrDados: TJsonArray;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados    := TJsonObject.Create;
      LJsonArrDados := TJsonArray.Create;
      try
        LJsonDados.Add('numero_nosso_numero').Value.AsString := ATitulo.NossoNumero;
        LJsonDados.Add('data_vencimento').Value.AsString := FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD');
        LJsonDados.Add('valor_titulo').Value.AsString := IntToStrZero(round(ATitulo.ValorDocumento * 100), 17);
        LJsonDados.Add('texto_uso_beneficiario').Value.AsString := '0';
        LJsonDados.Add('texto_seu_numero').Value.AsString := ATitulo.NossoNumero;
        LJsonArrDados.Add.AsObject                        := LJsonDados;

        LJsonPair := TJsonPair.Create(AJson, 'dados_individuais_boleto');
        try
          LJsonPair.Value.AsArray := LJsonArrDados;
          AJson.Add('dados_individuais_boleto').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonArrDados.Free;
        LJsonDados.Free;
      end;
    end;
  end;
end;


procedure TBoletoW_Itau_API.GerarDadosQrCode(AJson: TJsonObject);
var
  JsonDados: TJsonObject;
  JsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDados := TJsonObject.Create;
      try

        //ACBrPIXBase
        //JsonDados.Add('chave').Value.AsString := OnlyNumber(Boleto.Cedente.PIX.TipoChavePIX);
        JsonDados.Add('chave').Value.AsString := Trim(Boleto.Cedente.PIX.Chave);
        //JsonDados.Add('tipo_cobranca').Value.AsString := 'cob = cobrança pix imediata';

        JsonPair := TJsonPair.Create(AJson, 'dados_qrcode');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('dados_qrcode').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;


function TBoletoW_Itau_API.CodigoEspeciaDoc: string;
begin
    if AnsiSameText(ATitulo.EspecieDoc, 'DM') then
      Result := '01'
    else if AnsiSameText(ATitulo.EspecieDoc, 'NP') then
      Result := '02'
    else if AnsiSameText(ATitulo.EspecieDoc, 'NS') then
      Result := '03'
    else if AnsiSameText(ATitulo.EspecieDoc, 'ME') then
      Result := '04'
    else if AnsiSameText(ATitulo.EspecieDoc, 'RC') then
      Result := '05'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CT') then
      Result := '09'
    else if AnsiSameText(ATitulo.EspecieDoc, 'DS') then
      Result := '08'
    else if AnsiSameText(ATitulo.EspecieDoc, 'LC') then
      Result := '09'
    else if AnsiSameText(ATitulo.EspecieDoc, 'DD') then
      Result := '15'
    else if AnsiSameText(ATitulo.EspecieDoc, 'EC') then
      Result := '16'
    else if AnsiSameText(ATitulo.EspecieDoc, 'FS') or AnsiSameText(ATitulo.EspecieDoc, 'PS') then
      Result := '17'
    else if AnsiSameText(ATitulo.EspecieDoc, 'BDP') or AnsiSameText(ATitulo.EspecieDoc, 'BP') then
      Result := '18'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CBI') then
      Result := '88'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CC') then
      Result := '89'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CCB') then
      Result := '90'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CD') then
      Result := '91'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CH') then
      Result := '92'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CM') then
      Result := '93'
    else if AnsiSameText(ATitulo.EspecieDoc, 'CPS') then
      Result := '94'
    else if AnsiSameText(ATitulo.EspecieDoc, 'DMI') then
      Result := '95'
    else if AnsiSameText(ATitulo.EspecieDoc, 'DSI') then
      Result := '96'
    else if AnsiSameText(ATitulo.EspecieDoc, 'RA') then
      Result := '97'
    else if AnsiSameText(ATitulo.EspecieDoc, 'TA') then
      Result := '98'
    else
      Result := '99';

end;

procedure TBoletoW_Itau_API.GerarRecebimentoDivergente(AJson: TJsonObject);
var
  LJsonRecDivergente: TJsonObject;
  LJsonPairRecDivergente: TJsonPair;
begin
  if Assigned(ATitulo) then

  if Assigned(AJson) then
  begin
    LJsonRecDivergente := TJsonObject.Create;

    try
      LJsonRecDivergente.Add('codigo_tipo_autorizacao').Value.AsString := '0' + inttostr(Integer(ATitulo.TipoPagamento) + 1);
      if (Integer(ATitulo.TipoPagamento) <> 1) and (Integer(ATitulo.TipoPagamento) <> 3) then
      begin
        if (ATitulo.ValorMinPagamento > 0) and (ATitulo.ValorMaxPagamento > 0) then
          LJsonRecDivergente.Add('codigo_tipo_recebimento').Value.AsString := 'V'
        else
          LJsonRecDivergente.Add('codigo_tipo_recebimento').Value.AsString := 'P';
      end;
      if ATitulo.ValorMinPagamento > 0 then
        LJsonRecDivergente.Add('valor_minimo').Value.AsString := IntToStrZero(round(ATitulo.ValorMinPagamento * 100), 17)
      else
        if ATitulo.PercentualMinPagamento > 0 then
          LJsonRecDivergente.Add('percentual_minimo').Value.AsString := IntToStrZero(round(ATitulo.PercentualMinPagamento * 100000), 12);

      if ATitulo.ValorMinPagamento > 0 then
        LJsonRecDivergente.Add('valor_maximo').Value.AsString := IntToStrZero(round(ATitulo.ValorMaxPagamento * 100), 17)
      else
        if ATitulo.PercentualMaxPagamento > 0 then
          LJsonRecDivergente.Add('percentual_maximo').Value.AsString := IntToStrZero(round(ATitulo.PercentualMaxPagamento * 100000), 12);

      LJsonPairRecDivergente := TJsonPair.Create(AJson, 'recebimento_divergente');
      try
        LJsonPairRecDivergente.Value.AsObject := LJsonRecDivergente;
        AJson.Add('recebimento_divergente').Assign(LJsonPairRecDivergente);
      finally
        LJsonPairRecDivergente.Free;
      end;
    finally
      LJsonRecDivergente.Free;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarInstruCaoCobranca(AJson: TJsonObject);
var
  LJsonDados1, LJsonDados2, LJsonDados3: TJsonObject;
  LJsonArrDados: TJsonArray;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados1 := TJsonObject.Create;
      LJsonDados2 := TJsonObject.Create;
      LJsonDados3 := TJsonObject.Create;
      LJsonArrDados := TJsonArray.Create;
      try
        if (ATitulo.Instrucao1) <> '' then
        begin
          LJsonDados1.Add('codigo_instrucao_cobranca').Value.AsString := Copy(trim((ATitulo.Instrucao1)), 1, 2);
          LJsonDados1.Add('quantidade_dias_apos_vencimento').Value.AsString := Copy(trim((ATitulo.Instrucao1)), 3, 2);
          LJsonArrDados.Add.AsObject := LJsonDados1;
        end;
        if ATitulo.Instrucao2 <> '' then
        begin
          LJsonDados2.Add('codigo_instrucao_cobranca').Value.AsString := Copy(trim((ATitulo.Instrucao2)), 1, 2);
          LJsonDados2.Add('quantidade_dias_apos_vencimento').Value.AsString := Copy(trim((ATitulo.Instrucao2)), 3, 2);
          LJsonArrDados.Add.AsObject := LJsonDados2;
        end;
        if ATitulo.Instrucao3 <> '' then
        begin
          LJsonDados3.Add('codigo_instrucao_cobranca').Value.AsString := Copy(trim((ATitulo.Instrucao3)), 1, 2);
          LJsonDados3.Add('quantidade_dias_apos_vencimento').Value.AsString := Copy(trim((ATitulo.Instrucao3)), 3, 2);
          LJsonArrDados.Add.AsObject := LJsonDados3;
        end;

        LJsonPair := TJsonPair.Create(AJson, 'instrucao_cobranca');
        try
          LJsonPair.Value.AsArray := LJsonArrDados;
          AJson.Add('instrucao_cobranca').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonArrDados.Free;
        LJsonDados1.Free;
        LJsonDados2.Free;
        LJsonDados3.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarProtesto(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        LJsonDados.Add('protesto').Value.AsString := 'True';
        LJsonDados.Add('quantidade_dias_protesto').Value.AsInteger := ATitulo.DiasDeProtesto;
        LJsonPair := TJsonPair.Create(AJson, 'protesto');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('protesto').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarNegativacao(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        LJsonDados.Add('negativacao').Value.AsString := 'True';
        LJsonDados.Add('quantidade_dias_negativacao').Value.AsInteger := ATitulo.DiasDeNegativacao;
        LJsonPair := TJsonPair.Create(AJson, 'negativacao');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('negativacao').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GeraDadoBoleto(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  LJsonDados := TJsonObject.Create;
  try
    LJsonDados.Add('descricao_instrumento_cobranca').Value.AsString := ifthen(Boleto.Cedente.CedenteWS.IndicadorPix,'boleto_pix','boleto') ;
    LJsonDados.Add('tipo_boleto').Value.AsString := 'a vista';
    LJsonDados.Add('codigo_carteira').Value.AsInteger   := StrToIntDef(ATitulo.carteira, 0);
    LJsonDados.Add('valor_total_titulo').Value.AsString := IntToStrZero(round(ATitulo.ValorDocumento * 100), 17);
    LJsonDados.Add('codigo_especie').Value.AsString     := CodigoEspeciaDoc;
    LJsonDados.Add('valor_abatimento').Value.AsString   := IntToStrZero(round(ATitulo.ValorAbatimento * 100), 17);
    LJsonDados.Add('data_emissao').Value.AsString       := FormatDateBr(ATitulo.DataDocumento, 'yyyy-mm-dd');
    if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
      LJsonDados.Add('indicador_pagamento_parcial').Value.AsString := 'True'
    else
      LJsonDados.Add('indicador_pagamento_parcial').Value.AsString := 'False';
    LJsonDados.Add('quantidade_maximo_parcial').Value.AsInteger := 0;
    LJsonDados.Add('desconto_expresso').Value.AsString := 'False';

    GerarPagador(LJsonDados);
    GerarDadosIndividuaisBoleto(LJsonDados);
    GerarMulta(LJsonDados);
    GerarJuros(LJsonDados);
    GerarDesconto(LJsonDados);
    GerarRecebimentoDivergente(LJsonDados);
    if ATitulo.Instrucao1 <> '' then
      GerarInstruCaoCobranca(LJsonDados);

    if ATitulo.DiasDeProtesto > 0 then
      GerarProtesto(LJsonDados);
    if (ATitulo.DiasDeNegativacao > 0) then
      GerarNegativacao(LJsonDados);

    LJsonPair := TJsonPair.Create(AJson, 'dado_boleto');
    try
      LJsonPair.Value.AsObject := LJsonDados;
      AJson.Add('dado_boleto').Assign(LJsonPair);
    finally
      LJsonPair.Free;
    end;
  finally
    LJsonDados.Free;
  end;
end;

procedure TBoletoW_Itau_API.GerarData(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        LJsonDados.Add('etapa_processo_boleto').Value.AsString := ifthen(OAuth.Ambiente=taHomologacao,'validacao','efetivacao')  ;
        LJsonDados.Add('codigo_canal_operacao').Value.AsString := 'API';
        GeraIdBeneficiario(LJsonDados);
        GeraDadoBoleto(LJsonDados);
        LJsonPair := TJsonPair.Create(AJson, 'data');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('data').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
        finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.RequisicaoJson;
var
  LData: string;
  LJson: TJsonObject;
begin
  if Assigned(ATitulo) then
  begin
    LJson := TJsonObject.Create;
    try

      if Boleto.Cedente.CedenteWS.IndicadorPix then
      begin
        LJson.Add('etapa_processo_boleto').Value.AsString := ifthen(OAuth.Ambiente=taHomologacao,'simulacao','efetivacao');
        GeraIdBeneficiario(LJson);
        GeraDadoBoleto(LJson);
        GerarDadosQrCode(LJson);
      end
      else
      begin
        GerarData(LJson);
      end;

      LData := LJson.Stringify;

      FPDadosMsg := LData;

    finally
      LJson.Free;
    end;
  end;
end;


procedure TBoletoW_Itau_API.RequisicaoAltera;
var
  LData: string;
  LJson: TJsonObject;
begin
  if Assigned(ATitulo) then
  begin

    LJson := TJsonObject.Create;
    try

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3: // RemessaConcederAbatimento
          begin
            if (ATitulo.ValorAbatimento > 0) then
              LJson.Add('valor_abatimento').Value.AsNumber :=  ATitulo.ValorAbatimento;
          end;
        4: // RemessaCancelarAbatimento
          begin
            LJson.Add('valor_abatimento').Value.AsString := '0';
          end;
        5: //RemessaConcederDesconto
          begin
            AtribuirDesconto(LJson);
          end;
        7: //RemessaAlterarVencimento
          begin
              LJson.Add('data_vencimento').Value.AsString := FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD');
          end;
        9:  //RemessaProtestar
          begin
            AlterarProtesto(LJson);
          end;
        10:  //RemessaSustarProtesto
          begin
            AlterarProtesto(LJson);
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            AlterarProtesto(LJson);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            if (ATitulo.SeuNumero <> '') then
              LJson.Add('texto_seu_numero').Value.AsString := PadLeft(ATitulo.SeuNumero,10,'0')
            else
              raise Exception.Create(ACBrStr('Seu número é a identificação do boleto que poderá ' + sLineBreak +
                                 ' ter letras e números e OBRIGATÓRIAMENTE 10 posições' + sLineBreak));
          end;

        37: //RemessaCobrarJurosMora
          begin
            AtribuirJuros(LJson);
          end;
        50:  //RemessaAlterarMulta
          begin
             AtribuirMulta(LJson);
          end;
        51:  //RemessaDispensarMulta
          begin
             AtribuirMulta(LJson);
          end;
        52: //RemessaAlterarDesconto
          begin
            AtribuirDesconto(LJson);
          end;
        53: //toRemessaNaoConcederDesconto
          begin
            AtribuirDesconto(LJson);
          end;
        55:  //toRemessaAlterarPrazoLimiteRecebimento
          begin
            if (ATitulo.DataLimitePagto > 0) then
              LJson.Add('data_limite_pagamento').Value.AsString := FormatDateBr(ATitulo.DataLimitePagto, 'YYYY-MM-DD');
          end;
      end;

      LData := LJson.Stringify;

      FPDadosMsg := LData;

    finally
      LJson.Free;
    end;

  end;

end;

procedure TBoletoW_Itau_API.RequisicaoBaixa;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Itau_API.RequisicaoConsulta;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Itau_API.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Itau_API.GerarPessoaPg(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        LJsonDados.Add('nome_pessoa').Value.AsString :=
          ATitulo.Sacado.NomeSacado;
        GerarTipoPessoaPg(LJsonDados);

        LJsonPair := TJsonPair.Create(AJson, 'pessoa');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('pessoa').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarTipoPessoaPg(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        if Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) < 12 then
        begin
          LJsonDados.Add('codigo_tipo_pessoa').Value.AsString := 'F';
          LJsonDados.Add('numero_cadastro_pessoa_fisica').Value.AsString := OnlyNumber(ATitulo.Sacado.CNPJCPF);
        end
        else
        begin
          LJsonDados.Add('codigo_tipo_pessoa').Value.AsString := 'J';
          LJsonDados.Add('numero_cadastro_nacional_pessoa_juridica').Value.AsString := OnlyNumber(ATitulo.Sacado.CNPJCPF);
        end;
        LJsonPair := TJsonPair.Create(AJson, 'tipo_pessoa');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('tipo_pessoa').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarEnderecoPg(AJson: TJsonObject);
var
  LJsonDados: TJsonObject;
  LJsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TJsonObject.Create;
      try
        LJsonDados.Add('nome_logradouro').Value.AsString := ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero;
        LJsonDados.Add('nome_bairro').Value.AsString     := ATitulo.Sacado.Bairro;
        LJsonDados.Add('nome_cidade').Value.AsString     := ATitulo.Sacado.Cidade;
        LJsonDados.Add('sigla_UF').Value.AsString        := ATitulo.Sacado.UF;
        LJsonDados.Add('numero_CEP').Value.AsString      := ATitulo.Sacado.CEP;
        LJsonPair := TJsonPair.Create(AJson, 'endereco');
        try
          LJsonPair.Value.AsObject := LJsonDados;
          AJson.Add('endereco').Assign(LJsonPair);
        finally
          LJsonPair.Free;
        end;
      finally
        LJsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarPagador(AJson: TJsonObject);
var
  LJsonDadosPagador: TJsonObject;
  LJsonPairPagador: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDadosPagador := TJsonObject.Create;
      try
        GerarPessoaPg(LJsonDadosPagador);
        GerarEnderecoPg(LJsonDadosPagador);
        LJsonPairPagador := TJsonPair.Create(AJson, 'pagador');
        try
          LJsonPairPagador.Value.AsObject := LJsonDadosPagador;
          AJson.Add('pagador').Assign(LJsonPairPagador);
        finally
          LJsonPairPagador.Free;
        end;
      finally
        LJsonDadosPagador.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarJuros(AJson: TJsonObject);
var
  LJsonJuros: TJsonObject;
  LJsonPairJuros: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonJuros := TJsonObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if ATitulo.CodigoMora = '' then
          begin
            ATitulo.CodigoMora := '90';
            case ATitulo.CodigoMoraJuros of
              cjValorDia:
                ATitulo.CodigoMora := '93';
              cjTaxaDiaria:
                ATitulo.CodigoMora := '91';
              cjValorMensal:
                ATitulo.CodigoMora := '90';
              cjIsento:
                ATitulo.CodigoMora := '05';
            else
              ATitulo.CodigoMora := '05';
            end; //0 cjValorDia,1 cjTaxaMensal,2 cjIsento,3 cjValorMensal,4 cjTaxaDiaria
          end;

          LJsonJuros.Add('codigo_tipo_juros').Value.AsString := ATitulo.CodigoMora;
          LJsonJuros.Add('quantidade_dias_juros').Value.AsInteger := trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento);
          if ATitulo.CodigoMora = '93' then
          begin
            LJsonJuros.Add('valor_juros').Value.AsString := IntToStrZero(round(ATitulo.ValorMoraJuros * 100), 17)
          end
          else if (ATitulo.CodigoMora = '91') or (ATitulo.CodigoMora = '90') or (ATitulo.CodigoMora = '92') then
            LJsonJuros.Add('percentual_juros').Value.AsString := IntToStrZero(round(ATitulo.ValorMoraJuros * 100000), 12);

          LJsonPairJuros := TJsonPair.Create(AJson, 'juros');
          try
            LJsonPairJuros.Value.AsObject := LJsonJuros;
            AJson.Add('juros').Assign(LJsonPairJuros);
          finally
            LJsonPairJuros.Free;
          end;
        end;
      finally
        LJsonJuros.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarMulta(AJson: TJsonObject);
var
  LJsonMulta: TJsonObject;
  LJsonPairMulta: TJsonPair;
  LCodMulta: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonMulta := TJsonObject.Create;
      try
        if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
        begin
          LCodMulta := IfThen(ATitulo.MultaValorFixo,'01','02');
        end
        else
          LCodMulta := '03';

        LJsonMulta.Add('codigo_tipo_multa').Value.AsString := LCodMulta;
        if (ATitulo.DataMulta > 0) then
        begin
          if LCodMulta = '01' then
            LJsonMulta.Add('valor_multa').Value.AsString := IntToStrZero(round(ATitulo.PercentualMulta * 100), 17)
          else if LCodMulta = '02' then
            LJsonMulta.Add('percentual_multa').Value.AsString := IntToStrZero(round(ATitulo.PercentualMulta * 100000), 12);
          LJsonMulta.Add('quantidade_dias_multa').Value.AsInteger := trunc(ATitulo.DataMulta - ATitulo.Vencimento);
        end;

        LJsonPairMulta := TJsonPair.Create(AJson, 'multa');
        try
          LJsonPairMulta.Value.AsObject := LJsonMulta;
          AJson.Add('multa').Assign(LJsonPairMulta);
        finally
          LJsonPairMulta.Free;
        end;
      finally
        LJsonMulta.Free;
      end;
    end;
  end;
end;

function TBoletoW_Itau_API.TipoDescontoToString(const AValue: TACBrTipoDesconto): string;
begin
  Result := '00';
  case AValue of
    tdNaoConcederDesconto:
      Result := '00';
    tdValorFixoAteDataInformada:
      Result := '01';
    tdPercentualAteDataInformada:
      Result := '02';
    tdValorAntecipacaoDiaCorrido:
      Result := '03';
    tdValorAntecipacaoDiaUtil:
      Result := '91';
    tdPercentualSobreValorNominalDiaCorrido:
      Result := '90';
    tdPercentualSobreValorNominalDiaUtil:
      Result := '90';
    tdCancelamentoDesconto:
      Result := '07';
  end;
end;

procedure TBoletoW_Itau_API.GerarDesconto(AJson: TJsonObject);
var
  LJsonDesconto: TJsonObject;
  LJsonPairDesconto: TJsonPair;
  LJsonArrGrupoDesconto: TJsonArray;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDesconto := TJsonObject.Create;
      LJsonArrGrupoDesconto := TJsonArray.Create;
      try
        LJsonDesconto.Add('codigo_tipo_desconto').Value.AsString := TipoDescontoToString(ATitulo.TipoDesconto);
        if ATitulo.DataDesconto > 0 then
        begin
          LJsonDesconto.Add('data_desconto').Value.AsString := FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto);
          if ((Integer(ATitulo.TipoDesconto)) in [1, 3, 4]) then
            LJsonDesconto.Add('valor_desconto').Value.AsString := IntToStrZero(round(ATitulo.ValorDesconto * 100), 17)
          else
            LJsonDesconto.Add('percentual_desconto').Value.AsString := IntToStrZero(round(ATitulo.ValorDesconto * 100000), 12);
        end;
        LJsonArrGrupoDesconto.Add.AsObject := LJsonDesconto;

        // Desconto2
        if ATitulo.ValorDesconto2 > 0 then
        begin
          LJsonDesconto.Clear;
          LJsonDesconto.Add('codigo_tipo_desconto').Value.AsString := TipoDescontoToString(ATitulo.TipoDesconto2);
          if ATitulo.DataDesconto2 > 0 then
          begin
            LJsonDesconto.Add('data_desconto').Value.AsString := FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto2);
            if ((Integer(ATitulo.TipoDesconto2)) in [1, 3, 4]) then
              LJsonDesconto.Add('valor_desconto').Value.AsString := IntToStrZero(round(ATitulo.ValorDesconto2 * 100), 17)
            else
              LJsonDesconto.Add('percentual_desconto').Value.AsString := IntToStrZero(round(ATitulo.ValorDesconto2 * 100000), 12);
          end;
          LJsonArrGrupoDesconto.Add.AsObject := LJsonDesconto;
        end;

        LJsonPairDesconto := TJsonPair.Create(AJson, 'desconto ');
        try
          LJsonPairDesconto.Value.AsArray := LJsonArrGrupoDesconto;
          AJson.Add('desconto ').Assign(LJsonPairDesconto);
        finally
          LJsonPairDesconto.Free;
        end;

      finally
        LJsonArrGrupoDesconto.Free;
        LJsonDesconto.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.AtribuirDesconto(AJson: TJsonObject);
var
  LJsonAtribuirDesconto : TJsonObject;
  LJsonPairAtribuirDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonAtribuirDesconto := TJsonObject.Create;
      try
        case ATitulo.OcorrenciaOriginal.Tipo of
          toRemessaCancelarDesconto,toRemessaNaoConcederDesconto:
          begin
            LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '00'; // Quando não houver condição de desconto ou efetuar exclusão de desconto.
          end
        else
          begin
            if (ATitulo.ValorDesconto > 0) then
            begin
              case ATitulo.TipoDesconto of
                tdNaoConcederDesconto                       : LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '00';  // Quando não houver condição de desconto ou efetuar exclusão de desconto.
                tdValorFixoAteDataInformada                 : LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '01';  // Concede desconto de valor fixo, se o título for pago até a data estipulada
                tdPercentualAteDataInformada                : LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '02';  // Concede desconto percentual fixo, se o título for pago até a data estipulada
                tdValorAntecipacaoDiaCorrido                : LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '91';  // Até a data de vencimento, concede valor de desconto por dia antecipado de pagamento.
                tdPercentualSobreValorNominalDiaCorrido     : LJsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '90';  // Até a data de vencimento, concede desconto percentual por dia antecipado de pagamento.
              else
                raise Exception.Create(ACBrStr('TipoDesconto não é válido para este banco' + sLineBreak));
              end
            end
            else
            raise Exception.Create(ACBrStr('Valor do desconto é obrigatório' + sLineBreak));
            if Integer(ATitulo.TipoDesconto) > 0 then
               AtribuirDesconto1(LJsonAtribuirDesconto);
          end;
        end;

        LJsonPairAtribuirDesconto := TJsonPair.Create(AJson, 'desconto');
        try
          LJsonPairAtribuirDesconto.Value.AsObject := LJsonAtribuirDesconto;
          AJson.Add('desconto').Assign(LJsonPairAtribuirDesconto);
        finally
          LJsonPairAtribuirDesconto.Free;
        end;
      finally
        LJsonAtribuirDesconto.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.AtribuirDesconto1(AJson: TJsonObject);
var
  LJsonDesconto : TJsonObject;
  LJsonArrGrupoDesconto : TJsonArray;
  LJsonPairDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDesconto := TJSONObject.Create;
      LJsonArrGrupoDesconto := TJsonArray.Create;
      try
        if ATitulo.DataDesconto > 0 then
        begin
          if (ATitulo.TipoDesconto = tdValorFixoAteDataInformada) or (ATitulo.TipoDesconto = tdPercentualAteDataInformada) then
            LJsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto,ATitulo.Vencimento);

          if (ATitulo.ValorDesconto > 0) then
          begin
            case ATitulo.TipoDesconto of
              tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                LJsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto);
              tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                LJsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto);
            end;
          end
          else
            raise Exception.Create(ACBrStr('Obrigatório informar o valor do desconto!'));
          LJsonArrGrupoDesconto.Add.AsObject := LJsonDesconto;
        end;

        //Desconto2
        if ATitulo.DataDesconto2 > 0 then
        begin
          LJsonDesconto.Clear;
          LJsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto2,ATitulo.Vencimento);
          if (ATitulo.ValorDesconto2 > 0) then
          begin
            case ATitulo.TipoDesconto2 of
              tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                LJsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto2);
              tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                LJsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto2);
            end;
          end;
          LJsonArrGrupoDesconto.Add.AsObject := LJsonDesconto;
        end;

        //Desconto3
        if ATitulo.DataDesconto3 > 0 then
        begin
          LJsonDesconto.Clear;
          LJsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto3,ATitulo.Vencimento);
          if (ATitulo.ValorDesconto2 > 0) then
          begin
            case ATitulo.TipoDesconto2 of
              tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                LJsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto3);
              tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                LJsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto3);
            end;
          end;
          LJsonArrGrupoDesconto.Add.AsObject := LJsonDesconto;
        end;
        LJsonPairDesconto := TJsonPair.Create(AJson, 'descontos');
        try
          LJsonPairDesconto.Value.AsArray := LJsonArrGrupoDesconto;
          AJson.Add('descontos').Assign(LJsonPairDesconto);
        finally
          LJsonPairDesconto.Free;
        end;
      finally
        LJsonArrGrupoDesconto.Free;
        LJsonDesconto.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.AlterarProtesto(AJson: TJsonObject);
var
  LJsonAtribuirProtesto: TJsonObject;
  LJsonPairAtribuirProtesto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonAtribuirProtesto := TJsonObject.Create;
      try
        case ATitulo.OcorrenciaOriginal.Tipo of
          toRemessaProtestar :
            begin
              if (ATitulo.DiasDeProtesto > 0) then
              begin
                LJsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger     := 1;    //   protestar
                LJsonAtribuirProtesto.Add('quantidade_dias_protesto').Value.AsInteger := ATitulo.DiasDeProtesto;
              end
              else
              raise Exception.Create(ACBrStr('DiasDeProtesto obrigatório !'));
            end;
          toRemessaSustarProtesto:
            LJsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger         := 4; // nao protestar
          toRemessaCancelarInstrucaoProtesto:
            LJsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger         := 9; // cancelar protesto
        end;
        LJsonPairAtribuirProtesto := TJsonPair.Create(AJson, 'protesto');
        try
          LJsonPairAtribuirProtesto.Value.AsObject := LJsonAtribuirProtesto;
          AJson.Add('protesto').Assign(LJsonPairAtribuirProtesto);
        finally
          LJsonPairAtribuirProtesto.Free;
        end;
      finally
        LJsonAtribuirProtesto.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.AtribuirJuros(AJson: TJsonObject);
var
  LJsonJuros: TJsonObject;
  LJsonPairJuros: TJsonPair;
  LCodigoMora: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonJuros := TJsonObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if ATitulo.CodigoMora = '' then
          begin
            LCodigoMora := '90';
            case ATitulo.CodigoMoraJuros of
              cjValorDia:
                LCodigoMora := '93';
              cjTaxaMensal:
                LCodigoMora := '90';
              else
              raise Exception.Create(ACBrStr('CodigoMoraJuros permitido somente ' + sLineBreak + ' cjValorDia ou cjTaxaMensal !' + sLineBreak));
            end;
          end;
          LJsonJuros.Add('codigo_tipo_juros').Value.AsString := LCodigoMora;
          LJsonJuros.Add('quantidade_dias_juros').Value.AsInteger := trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento);
          if LCodigoMora = '93' then
          begin
            LJsonJuros.Add('valor_juros').Value.AsString := CurrToStrItau(ATitulo.ValorMoraJuros) ;
          end
          else if (LCodigoMora = '90') then
            LJsonJuros.Add('percentual_juros').Value.AsString := PercToStrItau(ATitulo.ValorMoraJuros);

          LJsonPairJuros := TJsonPair.Create(AJson, 'juros');
          try
            LJsonPairJuros.Value.AsObject := LJsonJuros;
            AJson.Add('juros').Assign(LJsonPairJuros);
          finally
            LJsonPairJuros.Free;
          end;
        end;
      finally
        LJsonJuros.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.AtribuirMulta(AJson: TJsonObject);
var
  LJsonMulta: TJsonObject;
  LJsonPairMulta: TJsonPair;
  LCodMulta: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonMulta := TJsonObject.Create;
      try
        case ATitulo.OcorrenciaOriginal.Tipo of
        toRemessaAlterarMulta :
          begin
            if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
              LCodMulta := IfThen(ATitulo.MultaValorFixo,'01','02')
            else
              raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta ou PercentualMulta não foi informada ! ' + sLineBreak));

            LJsonMulta.Add('codigo_tipo_multa').Value.AsString := LCodMulta;

            if (ATitulo.DataMulta > 0) then
            begin

              if LCodMulta = '01' then
                LJsonMulta.Add('valor_multa').Value.AsString := CurrToStrItau(ATitulo.PercentualMulta)
              else if LCodMulta = '02' then
                LJsonMulta.Add('percentual_multa').Value.AsString := PercToStrItau(ATitulo.PercentualMulta);

              LJsonMulta.Add('quantidade_dias_multa').Value.AsInteger := trunc(ATitulo.DataMulta - ATitulo.Vencimento);
            end
            else
              raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta não foi informada ! '));
          end;
        toRemessaDispensarMulta :
          LJsonMulta.Add('codigo_tipo_multa').Value.AsString := '03'; // dispensar multa
        end;

        LJsonPairMulta := TJsonPair.Create(AJson, 'multa');
        try
          LJsonPairMulta.Value.AsObject := LJsonMulta;
          AJson.Add('multa').Assign(LJsonPairMulta);
        finally
          LJsonPairMulta.Free;
        end;
      finally
        LJsonMulta.Free;
      end;
    end;
  end;
end;

function TBoletoW_Itau_API.CodigoTipoTitulo(AEspecieDoc: String): Integer;
begin
  { Pegando o tipo de EspecieDoc }
  if AEspecieDoc = 'CH' then
    AEspecieDoc := '01'
  else if AEspecieDoc = 'DM' then
    AEspecieDoc := '02'
  else if AEspecieDoc = 'DMI' then
    AEspecieDoc := '03'
  else if AEspecieDoc = 'DS' then
    AEspecieDoc := '04'
  else if AEspecieDoc = 'DSI' then
    AEspecieDoc := '05'
  else if AEspecieDoc = 'DR' then
    AEspecieDoc := '06'
  else if AEspecieDoc = 'LC' then
    AEspecieDoc := '07'
  else if AEspecieDoc = 'NCC' then
    AEspecieDoc := '08'
  else if AEspecieDoc = 'NCE' then
    AEspecieDoc := '09'
  else if AEspecieDoc = 'NCI' then
    AEspecieDoc := '10'
  else if AEspecieDoc = 'NCR' then
    AEspecieDoc := '11'
  else if AEspecieDoc = 'NP' then
    AEspecieDoc := '12'
  else if AEspecieDoc = 'NPR' then
    AEspecieDoc := '13'
  else if AEspecieDoc = 'TM' then
    AEspecieDoc := '14'
  else if AEspecieDoc = 'TS' then
    AEspecieDoc := '15'
  else if AEspecieDoc = 'NS' then
    AEspecieDoc := '16'
  else if AEspecieDoc = 'RC' then
    AEspecieDoc := '17'
  else if AEspecieDoc = 'FAT' then
    AEspecieDoc := '18'
  else if AEspecieDoc = 'ND' then
    AEspecieDoc := '19'
  else if AEspecieDoc = 'AP' then
    AEspecieDoc := '20'
  else if AEspecieDoc = 'ME' then
    AEspecieDoc := '21'
  else if AEspecieDoc = 'PC' then
    AEspecieDoc := '22';
  Result := StrToIntDef(AEspecieDoc, 0);
end;

constructor TBoletoW_Itau_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := True;
  end;

end;

function TBoletoW_Itau_API.CurrToStrItau(const AValue: Real): String;
begin
 result := StringReplace(FormatFloat('0.00', AValue), ',', '.', [rfReplaceAll])
end;

function TBoletoW_Itau_API.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;
end;

function TBoletoW_Itau_API.Enviar: boolean;
begin
  Result := inherited Enviar;
end;

function TBoletoW_Itau_API.GerarUUID: string;
var
  guid: TGUID;
begin
  if CreateGUID(guid) = S_OK then
    Result := GUIDToString(guid)
  else
    Result := '';
end;
end.
