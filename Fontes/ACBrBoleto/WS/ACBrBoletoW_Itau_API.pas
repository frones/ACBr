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
  ACBrJSON,
  ACBrUtil.Base,
  ACBrBoletoWS.Rest;

type

  { TBoletoW_Itau_API }
  TBoletoW_Itau_API = class(TBoletoWSREST)
  private
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
    procedure GerarData(AJson: TACBrJSONObject);
    procedure GeraIdBeneficiario(AJson: TACBrJSONObject);
    procedure GerarDadosQrCode(AJson: TACBrJSONObject);
    procedure GeraDadoBoleto(AJson: TACBrJSONObject);
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarPessoaPg(AJson: TACBrJSONObject);
    procedure GerarTipoPessoaPg(AJson: TACBrJSONObject);
    procedure GerarEnderecoPg(AJson: TACBrJSONObject);
    procedure GerarDadosIndividuaisBoleto(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GerarRecebimentoDivergente(AJson: TACBrJSONObject);
    procedure GerarInstruCaoCobranca(AJson: TACBrJSONObject);
    procedure GerarProtesto(AJson: TACBrJSONObject);
    procedure GerarNegativacao(AJson: TACBrJSONObject);

    procedure RequisicaoAltera;
    procedure AtribuirDesconto(AJson: TACBrJSONObject);
    procedure AtribuirDesconto1(AJson: TACBrJSONObject);
    procedure AlterarProtesto(AJson: TACBrJSONObject);
    procedure AtribuirJuros(AJson: TACBrJSONObject);
    procedure AtribuirMulta(AJson: TACBrJSONObject);

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
  StrUtils,
  DateUtils,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

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

  if Assigned(ATitulo) then
    LCarteira := ATitulo.Carteira;

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
    try
      LConsulta.Delimiter := '&';
      case Boleto.Configuracoes.WebService.Operacao of
        tpConsulta :
          begin
            LConsulta.Add('id_beneficiario=' + LId_Beneficiario);

            if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0 then
              LConsulta.Add('data_inclusao=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio, 'YYYY-MM-DD'));

            if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
              LConsulta.Add('page=' + IntToStr(Trunc(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade)));
            
            LConsulta.Add('view=full');
          end;
        tpConsultaDetalhe :
          begin
            LConsulta.Add('id_beneficiario=' + LId_Beneficiario);

            if LCarteira <> EmptyStr then
              LConsulta.Add('codigo_carteira=' + LCarteira);

            if LNossoNumero <> EmptyStr then
               LConsulta.Add('nosso_numero=' + LNossoNumero);

            LConsulta.Add('view=full');
          end;
        tpAltera :
          begin
             LConsulta.Add(LId_Beneficiario+
                          LCarteira+
                          LNossoNumero+'/'+DefinirInstrucaoAlteracao );
          end;
        tpBaixa :
          begin
             LConsulta.Add(LId_Beneficiario+
                          LCarteira+
                          LNossoNumero+'/baixa');
          end;
      end;
      Result := LConsulta.DelimitedText;
    finally
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

procedure TBoletoW_Itau_API.GeraIdBeneficiario(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
  LId_Beneficiario, LDAC: string;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDados := TACBrJSONObject.Create;
      LDAC := IfThen(Boleto.Cedente.DigitoVerificadorAgenciaConta <> '',
                     Boleto.Cedente.DigitoVerificadorAgenciaConta,
                     Boleto.Cedente.ContaDigito);

      LId_Beneficiario := PadLeft(Boleto.Cedente.Agencia, 4, '0') +
        PadLeft(Boleto.Cedente.Conta, 7, '0') +
        PadLeft(LDAC, 1, '0');

      LJsonDados.AddPair('id_beneficiario',LId_Beneficiario);
      AJson.AddPair('beneficiario',LJsonDados);
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarDadosIndividuaisBoleto(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
  LJsonArray : TACBrJSONArray;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados    := TACBrJSONObject.Create;
    LJsonArray    := TACBrJSONArray.Create;
    LJsonDados.AddPair('numero_nosso_numero', ATitulo.NossoNumero);
    LJsonDados.AddPair('data_vencimento', FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD'));
    LJsonDados.AddPair('valor_titulo', IntToStrZero(round(ATitulo.ValorDocumento * 100), 17));
    LJsonDados.AddPair('texto_uso_beneficiario', '0');
    LJsonDados.AddPair('texto_seu_numero', IfThen(ATitulo.SeuNumero <> '',  ATitulo.SeuNumero, ATitulo.NossoNumero));
    LJsonArray.AddElementJSON(LJsonDados);
    AJson.AddPair('dados_individuais_boleto', LJsonArray);
  end;
end;

procedure TBoletoW_Itau_API.GerarDadosQrCode(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;

    LJsonDados.AddPair('chave',Trim(Boleto.Cedente.PIX.Chave));
    AJson.AddPair('dados_qrcode', LJsonDados);

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

procedure TBoletoW_Itau_API.GerarRecebimentoDivergente(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;

    LJsonDados.AddPair('codigo_tipo_autorizacao', '0' + inttostr(Integer(ATitulo.TipoPagamento) + 1));

    if (Integer(ATitulo.TipoPagamento) <> 1) and (Integer(ATitulo.TipoPagamento) <> 3) then
    begin
      if (ATitulo.ValorMinPagamento > 0) and (ATitulo.ValorMaxPagamento > 0) then
        LJsonDados.AddPair('codigo_tipo_recebimento', 'V')
      else
        LJsonDados.AddPair('codigo_tipo_recebimento', 'P');
    end;
    if ATitulo.ValorMinPagamento > 0 then
      LJsonDados.AddPair('valor_minimo', IntToStrZero(round(ATitulo.ValorMinPagamento * 100), 17))
    else
      if ATitulo.PercentualMinPagamento > 0 then
        LJsonDados.AddPair('percentual_minimo', IntToStrZero(round(ATitulo.PercentualMinPagamento * 100000), 12));

    if ATitulo.ValorMinPagamento > 0 then
      LJsonDados.AddPair('valor_maximo', IntToStrZero(round(ATitulo.ValorMaxPagamento * 100), 17))
    else
      if ATitulo.PercentualMaxPagamento > 0 then
        LJsonDados.AddPair('percentual_maximo', IntToStrZero(round(ATitulo.PercentualMaxPagamento * 100000), 12));

    AJson.AddPair('recebimento_divergente', LJsonDados);
  end;
end;

procedure TBoletoW_Itau_API.GerarInstruCaoCobranca(AJson: TACBrJSONObject);
var
  LJsonDados, LJsonDados2, LJsonDados3 : TACBrJSONObject;
  LJsonArray : TACBrJSONArray;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;
    LJsonArray := TACBrJSONArray.Create;
    if (ATitulo.Instrucao1) <> '' then
    begin
      LJsonDados.AddPair('codigo_instrucao_cobranca', Copy(trim((ATitulo.Instrucao1)), 1, 2));
      if Boleto.Cedente.CedenteWS.IndicadorPix then
        LJsonDados.AddPair('quantidade_dias_apos_vencimento', Copy(trim((ATitulo.Instrucao1)), 3, 2))
      else
        LJsonDados.AddPair('quantidade_dias_instrucao_cobranca', Copy(trim((ATitulo.Instrucao1)), 3, 2));
      LJsonDados.AddPair('dia_util', StrToBool(IfThen(ATitulo.TipoDiasProtesto = diUteis,'True','False')));
      LJsonArray.AddElementJSON(LJsonDados);
    end;
    if ATitulo.Instrucao2 <> '' then
    begin
      LJsonDados2 := TACBrJSONObject.Create;
      LJsonDados2.AddPair('codigo_instrucao_cobranca', Copy(trim((ATitulo.Instrucao2)), 1, 1));
      if Boleto.Cedente.CedenteWS.IndicadorPix then
        LJsonDados.AddPair('quantidade_dias_apos_vencimento', Copy(trim((ATitulo.Instrucao2)), 3, 2))
      else
        LJsonDados.AddPair('quantidade_dias_instrucao_cobranca', Copy(trim((ATitulo.Instrucao2)), 3, 2));
      LJsonDados2.AddPair('dia_util', StrToBool(IfThen(ATitulo.TipoDiasProtesto = diUteis,'True','False')));
      LJsonArray.AddElementJSON(LJsonDados2);
    end;
    if ATitulo.Instrucao3 <> '' then
    begin
      LJsonDados3 := TACBrJSONObject.Create;
      LJsonDados3.AddPair('codigo_instrucao_cobranca', Copy(trim((ATitulo.Instrucao3)), 1,1));
      if Boleto.Cedente.CedenteWS.IndicadorPix then
        LJsonDados.AddPair('quantidade_dias_apos_vencimento', Copy(trim((ATitulo.Instrucao3)), 3, 2))
      else
        LJsonDados.AddPair('quantidade_dias_instrucao_cobranca', Copy(trim((ATitulo.Instrucao3)), 3, 2));
      LJsonDados3.AddPair('dia_util', StrToBool(IfThen(ATitulo.TipoDiasProtesto = diUteis,'True','False')));
      LJsonArray.AddElementJSON(LJsonDados3);
    end;

    AJson.AddPair('instrucao_cobranca',LJsonArray);
  end;
end;

procedure TBoletoW_Itau_API.GerarProtesto(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson)then
  begin
    LJsonDados := TACBrJSONObject.Create;

    LJsonDados.AddPair('protesto', 'True');
    LJsonDados.AddPair('quantidade_dias_protesto', ATitulo.DiasDeProtesto);
    AJson.AddPair('protesto', LJsonDados);

  end;
end;

procedure TBoletoW_Itau_API.GerarNegativacao(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;

    LJsonDados.AddPair('negativacao', 'True');
    LJsonDados.AddPair('quantidade_dias_negativacao', ATitulo.DiasDeNegativacao);

    AJson.AddPair('negativacao', LJsonDados);

  end;
end;

procedure TBoletoW_Itau_API.GeraDadoBoleto(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  LJsonDados := TACBrJSONObject.Create;
  try
    LJsonDados.AddPair('descricao_instrumento_cobranca', IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'boleto_pix','boleto'));
    LJsonDados.AddPair('tipo_boleto', 'a vista');
    LJsonDados.AddPair('codigo_carteira', StrToIntDef(ATitulo.carteira, 0));
    LJsonDados.AddPair('valor_total_titulo', IntToStrZero(round(ATitulo.ValorDocumento * 100), 17));
    LJsonDados.AddPair('codigo_especie', CodigoEspeciaDoc);
    LJsonDados.AddPair('valor_abatimento', IntToStrZero(round(ATitulo.ValorAbatimento * 100), 17));
    LJsonDados.AddPair('data_emissao', FormatDateBr(ATitulo.DataDocumento, 'yyyy-mm-dd'));

    if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
      LJsonDados.AddPair('indicador_pagamento_parcial', 'True')
    else
      LJsonDados.AddPair('indicador_pagamento_parcial', 'False');

    LJsonDados.AddPair('quantidade_maximo_parcial', 0);
    LJsonDados.AddPair('desconto_expresso', 'False');

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

    AJson.AddPair('dado_boleto',LJsonDados);

  finally
    LJsonDados.Free;
  end;
end;

procedure TBoletoW_Itau_API.GerarData(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;
    LJsonDados.AddPair('etapa_processo_boleto', IfThen(OAuth.Ambiente=taHomologacao,'validacao','efetivacao'));
    LJsonDados.AddPair('codigo_canal_operacao', 'API');
    GeraIdBeneficiario(LJsonDados);
    GeraDadoBoleto(LJsonDados);
    AJson.AddPair('data',LJsonDados);
  end;
end;

procedure TBoletoW_Itau_API.RequisicaoJson;
var
  LJson: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJson := TACBrJSONObject.Create;
    if Boleto.Cedente.CedenteWS.IndicadorPix then
    begin
      LJson.AddPair('etapa_processo_boleto', IfThen(OAuth.Ambiente=taHomologacao,'simulacao','efetivacao'));
      GeraIdBeneficiario(LJson);
      GeraDadoBoleto(LJson);
      GerarDadosQrCode(LJson);
    end
    else
    begin
      GerarData(LJson);
    end;

    FPDadosMsg := LJson.ToJSON;
  end;
end;


procedure TBoletoW_Itau_API.RequisicaoAltera;
var
  LJson: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin

    LJson := TACBrJSONObject.Create;
    try

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3: // RemessaConcederAbatimento
          begin
            if (ATitulo.ValorAbatimento > 0) then
              LJson.AddPair('valor_abatimento', StringReplace(FormatFloat('0.00',ATitulo.ValorAbatimento),',','.',[]));
          end;
        4: // RemessaCancelarAbatimento
          begin
            LJson.AddPair('valor_abatimento', StringReplace(FormatFloat('0.00',0),',','.',[]));
          end;
        5: //RemessaConcederDesconto
          begin
            AtribuirDesconto(LJson);
          end;
        7: //RemessaAlterarVencimento
          begin
              LJson.AddPair('data_vencimento', FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD'));
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
              LJson.AddPair('texto_seu_numero', PadLeft(ATitulo.SeuNumero,10,'0'))
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
              LJson.AddPair('data_limite_pagamento', FormatDateBr(ATitulo.DataLimitePagto, 'YYYY-MM-DD'));
          end;
      end;

      FPDadosMsg := LJson.ToJSON;

    finally
      LJson.Free;
    end;
  end;
 end;

procedure TBoletoW_Itau_API.RequisicaoBaixa;
begin
  FPDadosMsg := '{}';
end;

procedure TBoletoW_Itau_API.RequisicaoConsulta;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Itau_API.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Itau_API.GerarPessoaPg(AJson: TacbrJsonObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;
    LJsonDados.AddPair('nome_pessoa', ATitulo.Sacado.NomeSacado);
    GerarTipoPessoaPg(LJsonDados);

    AJson.AddPair('pessoa',LJsonDados);
  end;
end;

procedure TBoletoW_Itau_API.GerarTipoPessoaPg(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TACBrJSONObject.Create;

    if Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) < 12 then
    begin
      LJsonDados.AddPair('codigo_tipo_pessoa', 'F');
      LJsonDados.AddPair('numero_cadastro_pessoa_fisica', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    end
    else
    begin
      LJsonDados.AddPair('codigo_tipo_pessoa','J');
      LJsonDados.AddPair('numero_cadastro_nacional_pessoa_juridica', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    end;

    AJson.AddPair('tipo_pessoa', LJsonDados);
  end;
end;

procedure TBoletoW_Itau_API.GerarEnderecoPg(AJson: TACBrJSONObject);
var
  LJsonDados: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDados := TacbrJsonObject.Create;

    LJsonDados.AddPair('nome_logradouro', ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero);
    LJsonDados.AddPair('nome_bairro', ATitulo.Sacado.Bairro);
    LJsonDados.AddPair('nome_cidade', ATitulo.Sacado.Cidade);
    LJsonDados.AddPair('sigla_UF', ATitulo.Sacado.UF);
    LJsonDados.AddPair('numero_CEP', ATitulo.Sacado.CEP);
    AJson.AddPair('endereco',LJsonDados);

  end;
end;

procedure TBoletoW_Itau_API.GerarPagador(AJson: TACBrJSONObject);
var
  LJsonDadosPagador: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDadosPagador := TacbrJsonObject.Create;

    GerarPessoaPg(LJsonDadosPagador);
    GerarEnderecoPg(LJsonDadosPagador);
    AJson.AddPair('pagador',LJsonDadosPagador);
  end;
end;

procedure TBoletoW_Itau_API.GerarJuros(AJson: TACBrJSONObject);
var
  LJsonJuros: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonJuros := TACBrJSONObject.Create;

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

      LJsonJuros.AddPair('codigo_tipo_juros', ATitulo.CodigoMora);
      LJsonJuros.AddPair('quantidade_dias_juros', trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento));
      if ATitulo.CodigoMora = '93' then
      begin
        LJsonJuros.AddPair('valor_juros', IntToStrZero(round(ATitulo.ValorMoraJuros * 100), 17))
      end
      else if (ATitulo.CodigoMora = '91') or (ATitulo.CodigoMora = '90') or (ATitulo.CodigoMora = '92') then
        LJsonJuros.AddPair('percentual_juros', IntToStrZero(round(ATitulo.ValorMoraJuros * 100000), 12));

      AJson.AddPair('juros',LJsonJuros);

    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarMulta(AJson: TACBrJSONObject);
var
  LJsonMulta: TACBrJSONObject;
  LCodMulta: String;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonMulta := TacbrJsonObject.Create;

    if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
    begin
      LCodMulta := IfThen(ATitulo.MultaValorFixo,'01','02');
    end
    else
      LCodMulta := '03';

    LJsonMulta.AddPair('codigo_tipo_multa', LCodMulta);
    if (ATitulo.DataMulta > 0) then
    begin
      if LCodMulta = '01' then
        LJsonMulta.AddPair('valor_multa', IntToStrZero(round(ATitulo.PercentualMulta * 100), 17))
      else if LCodMulta = '02' then
        LJsonMulta.AddPair('percentual_multa', IntToStrZero(round(ATitulo.PercentualMulta * 100000), 12));
      LJsonMulta.AddPair('quantidade_dias_multa', trunc(ATitulo.DataMulta - ATitulo.Vencimento));
    end;

    AJson.AddPair('multa',LJsonMulta);
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

procedure TBoletoW_Itau_API.GerarDesconto(AJson: TACBrJSONObject);
var
  LJsonDesconto,LJsonDesconto2: TACBrJSONObject;
  LJsonArray : TACBrJSONArray;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDesconto := TACBrJSONObject.Create;
    LJsonArray := TACBrJSONArray.Create;
    LJsonDesconto.AddPair('codigo_tipo_desconto', TipoDescontoToString(ATitulo.TipoDesconto));
    if ATitulo.DataDesconto > 0 then
    begin
      LJsonDesconto.AddPair('data_desconto', FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto));
      if ((Integer(ATitulo.TipoDesconto)) in [1, 3, 4]) then
        LJsonDesconto.AddPair('valor_desconto', IntToStrZero(round(ATitulo.ValorDesconto * 100), 17))
      else
        LJsonDesconto.AddPair('percentual_desconto', IntToStrZero(round(ATitulo.ValorDesconto * 100000), 12));
    end;
    LJsonArray.AddElementJSON(LJsonDesconto);

    // Desconto2
    if ATitulo.ValorDesconto2 > 0 then
    begin
      LJsonDesconto2 := TACBrJSONObject.Create;
      LJsonDesconto.AddPair('codigo_tipo_desconto', TipoDescontoToString(ATitulo.TipoDesconto2));
      if ATitulo.DataDesconto2 > 0 then
      begin
        LJsonDesconto.AddPair('data_desconto', FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto2));
        if ((Integer(ATitulo.TipoDesconto2)) in [1, 3, 4]) then
          LJsonDesconto.AddPair('valor_desconto', IntToStrZero(round(ATitulo.ValorDesconto2 * 100), 17))
        else
          LJsonDesconto.AddPair('percentual_desconto', IntToStrZero(round(ATitulo.ValorDesconto2 * 100000), 12));
      end;
      LJsonArray.AddElementJSON(LJsonDesconto2);
    end;

    AJson.AddPair('desconto ',LJsonArray);
  end;
end;

procedure TBoletoW_Itau_API.AtribuirDesconto(AJson: TACBrJSONObject);
var
  LJsonAtribuirDesconto : TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonAtribuirDesconto := TACBrJSONObject.Create;
    case ATitulo.OcorrenciaOriginal.Tipo of
      toRemessaCancelarDesconto,toRemessaNaoConcederDesconto:
      begin
        LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto','00'); // Quando não houver condição de desconto ou efetuar exclusão de desconto.
      end
    else
      begin
        if (ATitulo.ValorDesconto > 0) then
        begin
          case ATitulo.TipoDesconto of
            tdNaoConcederDesconto                       : LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto', '00');  // Quando não houver condição de desconto ou efetuar exclusão de desconto.
            tdValorFixoAteDataInformada                 : LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto', '01');  // Concede desconto de valor fixo, se o título for pago até a data estipulada
            tdPercentualAteDataInformada                : LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto', '02');  // Concede desconto percentual fixo, se o título for pago até a data estipulada
            tdValorAntecipacaoDiaCorrido                : LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto', '91');  // Até a data de vencimento, concede valor de desconto por dia antecipado de pagamento.
            tdPercentualSobreValorNominalDiaCorrido     : LJsonAtribuirDesconto.AddPair('codigo_tipo_desconto', '90');  // Até a data de vencimento, concede desconto percentual por dia antecipado de pagamento.
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

    AJson.AddPair('desconto',LJsonAtribuirDesconto);
  end;
end;

procedure TBoletoW_Itau_API.AtribuirDesconto1(AJson: TACBrJSONObject);
var
  LJsonDesconto, LJsonDesconto2, LJsonDesconto3 : TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDesconto := TACBrJSONObject.Create;
    LJsonArray := TACBrJSONArray.Create;

      if ATitulo.DataDesconto > 0 then
      begin
        if (ATitulo.TipoDesconto = tdValorFixoAteDataInformada) or (ATitulo.TipoDesconto = tdPercentualAteDataInformada) then
          LJsonDesconto.AddPair('quantidade_dias_desconto', DaysBetween(ATitulo.DataDesconto,ATitulo.Vencimento));

        if (ATitulo.ValorDesconto > 0) then
        begin
          case ATitulo.TipoDesconto of
            tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
              LJsonDesconto.AddPair('valor_desconto', CurrToStrItau(ATitulo.ValorDesconto));
            tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
              LJsonDesconto.AddPair('percentual_desconto', PercToStrItau(ATitulo.ValorDesconto));
          end;
        end
        else
          raise Exception.Create(ACBrStr('Obrigatório informar o valor do desconto!'));
        LJsonArray.AddElementJSON(LJsonDesconto);
      end;

      //Desconto2
      if ATitulo.DataDesconto2 > 0 then
      begin
        LJsonDesconto2 := TACBrJSONObject.Create;
        LJsonDesconto2.AddPair('quantidade_dias_desconto', DaysBetween(ATitulo.DataDesconto2,ATitulo.Vencimento));
        if (ATitulo.ValorDesconto2 > 0) then
        begin
          case ATitulo.TipoDesconto2 of
            tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
              LJsonDesconto2.AddPair('valor_desconto', CurrToStrItau(ATitulo.ValorDesconto2));
            tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
              LJsonDesconto2.AddPair('percentual_desconto', PercToStrItau(ATitulo.ValorDesconto2));
          end;
        end;
        LJsonArray.AddElementJSON(LJsonDesconto2);
      end;

      //Desconto3
      if ATitulo.DataDesconto3 > 0 then
      begin
        LJsonDesconto3 := TACBrJSONObject.Create;
        LJsonDesconto3.AddPair('quantidade_dias_desconto', DaysBetween(ATitulo.DataDesconto3,ATitulo.Vencimento));
        if (ATitulo.ValorDesconto2 > 0) then
        begin
          case ATitulo.TipoDesconto2 of
            tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
              LJsonDesconto3.AddPair('valor_desconto', CurrToStrItau(ATitulo.ValorDesconto3));
            tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
              LJsonDesconto3.AddPair('percentual_desconto', PercToStrItau(ATitulo.ValorDesconto3));
          end;
        end;
        LJsonArray.AddElementJSON(LJsonDesconto3);
      end;
    AJson.AddPair('descontos',LJsonArray);
  end;
end;

procedure TBoletoW_Itau_API.AlterarProtesto(AJson: TACBrJSONObject);
var
  LJsonAtribuirProtesto: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonAtribuirProtesto := TacbrJsonObject.Create;
    case ATitulo.OcorrenciaOriginal.Tipo of
      toRemessaProtestar :
        begin
          if (ATitulo.DiasDeProtesto > 0) then
          begin
            LJsonAtribuirProtesto.AddPair('codigo_tipo_protesto', 1);    //   protestar
            LJsonAtribuirProtesto.AddPair('quantidade_dias_protesto', ATitulo.DiasDeProtesto);
          end
          else
          raise Exception.Create(ACBrStr('DiasDeProtesto obrigatório !'));
        end;
      toRemessaSustarProtesto:
        LJsonAtribuirProtesto.AddPair('codigo_tipo_protesto', 4); // nao protestar
      toRemessaCancelarInstrucaoProtesto:
        LJsonAtribuirProtesto.AddPair('codigo_tipo_protesto', 9); // cancelar protesto
    end;
    AJson.AddPair('protesto', LJsonAtribuirProtesto);
  end;
end;

procedure TBoletoW_Itau_API.AtribuirJuros(AJson: TACBrJSONObject);
var
  LJsonJuros: TACBrJSONObject;
  LCodigoMora: String;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonJuros := TacbrJsonObject.Create;

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
      LJsonJuros.AddPair('codigo_tipo_juros', LCodigoMora);
      LJsonJuros.AddPair('quantidade_dias_juros', trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento));
      if LCodigoMora = '93' then
      begin
        LJsonJuros.AddPair('valor_juros', CurrToStrItau(ATitulo.ValorMoraJuros));
      end
      else if (LCodigoMora = '90') then
        LJsonJuros.AddPair('percentual_juros', PercToStrItau(ATitulo.ValorMoraJuros));

      AJson.AddPair('juros', LJsonJuros);
    end;
  end;
end;

procedure TBoletoW_Itau_API.AtribuirMulta(AJson: TACBrJSONObject);
var
  LJsonMulta: TACBrJSONObject;
  LCodMulta: String;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonMulta := TacbrJsonObject.Create;
    case ATitulo.OcorrenciaOriginal.Tipo of
    toRemessaAlterarMulta :
      begin
        if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
          LCodMulta := IfThen(ATitulo.MultaValorFixo,'01','02')
        else
          raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta ou PercentualMulta não foi informada ! ' + sLineBreak));

        LJsonMulta.AddPair('codigo_tipo_multa', LCodMulta);

        if (ATitulo.DataMulta > 0) then
        begin

          if LCodMulta = '01' then
            LJsonMulta.AddPair('valor_multa', CurrToStrItau(ATitulo.PercentualMulta))
          else if LCodMulta = '02' then
            LJsonMulta.AddPair('percentual_multa', PercToStrItau(ATitulo.PercentualMulta));

          LJsonMulta.AddPair('quantidade_dias_multa', trunc(ATitulo.DataMulta - ATitulo.Vencimento));
        end
        else
          raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta não foi informada ! '));
      end;
    toRemessaDispensarMulta :
      LJsonMulta.AddPair('codigo_tipo_multa', '03'); // dispensar multa
    end;

    AJson.AddPair('multa',LJsonMulta);
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
