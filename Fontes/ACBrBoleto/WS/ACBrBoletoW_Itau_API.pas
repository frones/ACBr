{****************************************************************************** }
{ Projeto: Componentes ACBr }
{ Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida }

{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales }

{ Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr }

{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }

{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }

{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{ Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170 }
{ ****************************************************************************** }

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
  C_URL =     'https://api.itau.com.br/cash_management/v2';
  C_URL_HOM = 'https://devportal.itau.com.br/sandboxapi/cash_management_ext_v2/v2';

  C_URL_CONSULTA = 'https://secure.api.cloud.itau.com.br/boletoscash/v2';

  C_URL_OAUTH_PROD = 'https://sts.itau.com.br/api/oauth/token';
  C_URL_OAUTH_HOM = 'https://devportal.itau.com.br/api/jwt';

  C_ACCEPT = ''; // 'application/json';
  // C_ACCEPT         = '*/*'; //'application/json';

  Protocolo = '1.1';
  C_AUTHORIZATION_HOM = 'x-sandbox-token';
  C_AUTHORIZATION = 'Authorization';

implementation

uses
  synacode, strutils, DateUtils, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TBoletoW_Itau_API }

procedure TBoletoW_Itau_API.DefinirURL;
begin

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM);
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui: FPURL := FPURL + '/boletos';

    tpConsulta:
      FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,
        C_URL_CONSULTA, C_URL_HOM) + '/boletos?' + DefinirParametros;

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
  FPAuthorization :=
    IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,
    C_AUTHORIZATION, C_AUTHORIZATION_HOM) + ' : ' + GerarTokenAutenticacao;
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
 if Assigned(ATitulo) then
  FPKeyUser := '';
  FPKeyUser := 'x-itau-apikey: ' + Boleto.Cedente.CedenteWS.ClientID + #13#10 +
    'x-itau-flowID: 1' + #13#10 +
    'x-itau-correlationID: ' + GerarUUID;
    //'8A30E15C-1AC9-418A-AF1E-02E36BF77ADE'
end;

function TBoletoW_Itau_API.DefinirParametros: String;
var
  LNossoNumero, LId_Beneficiario, LCarteira, Documento: String;
  Consulta: TStringList;

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

    if (Boleto.Cedente.DigitoVerificadorAgenciaConta = EmptyStr) then
      raise EACBrBoletoWSException.Create
        (ClassName +
        ' Obrigatório informar o DigitoVerificadorAgenciaContaBeneficiario. ');

    LId_Beneficiario := PadLeft(Boleto.Cedente.Agencia, 4, '0') +
                        PadLeft(Boleto.Cedente.Conta, 7, '0') +
                        PadLeft(Boleto.Cedente.DigitoVerificadorAgenciaConta, 1, '0');

    Consulta := TStringList.Create;
    Consulta.Delimiter := '&';
    try
      case Boleto.Configuracoes.WebService.Operacao of
        tpConsulta,tpConsultaDetalhe :
          begin
            Consulta.Add('id_beneficiario=' + LId_Beneficiario);

            if Boleto.Configuracoes.WebService.Filtro.carteira > 0 then
              Consulta.Add('codigo_carteira=' +
                inttostr(Boleto.Configuracoes.WebService.Filtro.carteira));

            if LNossoNumero <> EmptyStr then
               Consulta.Add('nosso_numero=' + LNossoNumero);

            if Boleto.Configuracoes.WebService.Operacao = tpConsulta then
              if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0
              then
                Consulta.Add('data_inclusao=' +
                  FormatDateBr(Boleto.Configuracoes.WebService.Filtro.
                  dataVencimento.DataInicio, 'YYYY-MM-DD'));
          end;
        tpAltera :
          begin
             Consulta.Add(LId_Beneficiario+
                          ATitulo.Carteira+
                          LNossoNumero+'/'+DefinirInstrucaoAlteracao );
          end;
        tpBaixa :
          begin
             Consulta.Add(LId_Beneficiario+
                          inttostr(Boleto.Configuracoes.WebService.Filtro.carteira)+
                          LNossoNumero+'/baixa');
          end;


      end;

    finally
      Result := Consulta.DelimitedText;
      Consulta.Free;
    end;

  end;

end;

procedure TBoletoW_Itau_API.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Itau_API.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef
    (IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1',
    '2'), 2);
end;

procedure TBoletoW_Itau_API.GeraIdBeneficiario(AJson: TJsonObject);
var
  JsonDados: TJsonObject;
  JsonPair: TJsonPair;
  LId_Beneficiario: string;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDados := TJsonObject.Create;
      try
        LId_Beneficiario := PadLeft(Boleto.Cedente.Agencia, 4, '0') +
          PadLeft(Boleto.Cedente.Conta, 7, '0') +
          PadLeft(Boleto.Cedente.DigitoVerificadorAgenciaConta, 1, '0');

        JsonDados.Add('id_beneficiario').Value.AsString := LId_Beneficiario;

        JsonPair := TJsonPair.Create(AJson, 'beneficiario');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('beneficiario').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarDadosIndividuaisBoleto(AJson: TJsonObject);
var
  JsonDados: TJsonObject;
  JsonPair: TJsonPair;
  JsonArrDados: TJsonArray;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDados := TJsonObject.Create;
      JsonArrDados := TJsonArray.Create;
      try
        JsonDados.Add('numero_nosso_numero').Value.AsString :=
          ATitulo.NossoNumero;
        JsonDados.Add('data_vencimento').Value.AsString :=
          FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD');
        JsonDados.Add('valor_titulo').Value.AsString :=
          IntToStrZero(round(ATitulo.ValorDocumento * 100), 17);
        JsonDados.Add('texto_uso_beneficiario').Value.AsString := '0';
        JsonDados.Add('texto_seu_numero').Value.AsString := ATitulo.NossoNumero;
        JsonArrDados.Add.AsObject := JsonDados;

        JsonPair := TJsonPair.Create(AJson, 'dados_individuais_boleto');
        try
          JsonPair.Value.AsArray := JsonArrDados;
          AJson.Add('dados_individuais_boleto').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonArrDados.Free;
        JsonDados.Free;
      end;
    end;
  end;
end;

function TBoletoW_Itau_API.CodigoEspeciaDoc: string;
begin
  with ATitulo do
  begin
    if AnsiSameText(EspecieDoc, 'DM') then
      Result := '01'
    else if AnsiSameText(EspecieDoc, 'NP') then
      Result := '02'
    else if AnsiSameText(EspecieDoc, 'NS') then
      Result := '03'
    else if AnsiSameText(EspecieDoc, 'CS') then
      Result := '04'
    else if AnsiSameText(EspecieDoc, 'REC') then
      Result := '05'
    else if AnsiSameText(EspecieDoc, 'LC') then
      Result := '09'
    else if AnsiSameText(EspecieDoc, 'DS') then
      Result := '08'
    else if AnsiSameText(EspecieDoc, 'ND') then
      Result := '13'
    else if AnsiSameText(EspecieDoc, 'PS') then
      Result := '17'
    else if AnsiSameText(EspecieDoc, 'BP') then
      Result := '18'
    else
      Result := '99';
  end;
end;

procedure TBoletoW_Itau_API.GerarRecebimentoDivergente(AJson: TJsonObject);
var
  JsonRecDivergente: TJsonObject;
  JsonPairRecDivergente: TJsonPair;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonRecDivergente := TJsonObject.Create;

        try
          JsonRecDivergente.Add('codigo_tipo_autorizacao').Value.AsString :=
            '0' + inttostr(Integer(TipoPagamento) + 1);
          if (Integer(TipoPagamento) <> 1) and (Integer(TipoPagamento) <> 3)
          then
          begin
            if (ValorMinPagamento > 0) and (ValorMaxPagamento > 0) then
              JsonRecDivergente.Add('codigo_tipo_recebimento')
                .Value.AsString := 'V'
            else
              JsonRecDivergente.Add('codigo_tipo_recebimento')
                .Value.AsString := 'P';
          end;
          if ValorMinPagamento > 0 then
            JsonRecDivergente.Add('valor_minimo').Value.AsString :=
              IntToStrZero(round(ValorMinPagamento * 100), 17)
          else if PercentualMinPagamento > 0 then
            JsonRecDivergente.Add('percentual_minimo').Value.AsString :=
              IntToStrZero(round(PercentualMinPagamento * 100000), 12);

          if ValorMinPagamento > 0 then
            JsonRecDivergente.Add('valor_maximo').Value.AsString :=
              IntToStrZero(round(ValorMaxPagamento * 100), 17)
          else if PercentualMaxPagamento > 0 then
            JsonRecDivergente.Add('percentual_maximo').Value.AsString :=
              IntToStrZero(round(PercentualMaxPagamento * 100000), 12);

          JsonPairRecDivergente := TJsonPair.Create(AJson,
            'recebimento_divergente');
          try
            JsonPairRecDivergente.Value.AsObject := JsonRecDivergente;
            AJson.Add('recebimento_divergente').Assign(JsonPairRecDivergente);
          finally
            JsonPairRecDivergente.Free;
          end;
        finally
          JsonRecDivergente.Free;
        end;
      end;

    end;
end;

procedure TBoletoW_Itau_API.GerarInstruCaoCobranca(AJson: TJsonObject);
var
  JsonDados1, JsonDados2, JsonDados3: TJsonObject;
  JsonArrDados: TJsonArray;
  JsonPair: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDados1 := TJsonObject.Create;
      JsonDados2 := TJsonObject.Create;
      JsonDados3 := TJsonObject.Create;
      JsonArrDados := TJsonArray.Create;
      try
        if (ATitulo.Instrucao1) <> '' then
        begin
          JsonDados1.Add('codigo_instrucao_cobranca').Value.AsString :=
            Copy(trim((ATitulo.Instrucao1)), 1, 2);
          JsonDados1.Add('quantidade_dias_apos_vencimento').Value.AsString :=
            Copy(trim((ATitulo.Instrucao1)), 3, 2);
          JsonArrDados.Add.AsObject := JsonDados1;
        end;
        if ATitulo.Instrucao2 <> '' then
        begin
          JsonDados2.Add('codigo_instrucao_cobranca').Value.AsString :=
            Copy(trim((ATitulo.Instrucao2)), 1, 2);
          JsonDados2.Add('quantidade_dias_apos_vencimento').Value.AsString :=
            Copy(trim((ATitulo.Instrucao2)), 3, 2);
          JsonArrDados.Add.AsObject := JsonDados2;
        end;
        if ATitulo.Instrucao3 <> '' then
        begin
          JsonDados3.Add('codigo_instrucao_cobranca').Value.AsString :=
            Copy(trim((ATitulo.Instrucao3)), 1, 2);
          JsonDados3.Add('quantidade_dias_apos_vencimento').Value.AsString :=
            Copy(trim((ATitulo.Instrucao3)), 3, 2);
          JsonArrDados.Add.AsObject := JsonDados3;
        end;

        JsonPair := TJsonPair.Create(AJson, 'instrucao_cobranca');
        try
          JsonPair.Value.AsArray := JsonArrDados;
          AJson.Add('instrucao_cobranca').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonArrDados.Free;
        JsonDados1.Free;
        JsonDados2.Free;
        JsonDados3.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarProtesto(AJson: TJsonObject);
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
        JsonDados.Add('protesto').Value.AsString := 'True';
        JsonDados.Add('quantidade_dias_protesto').Value.AsInteger :=
          ATitulo.DiasDeProtesto;
        JsonPair := TJsonPair.Create(AJson, 'protesto');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('protesto').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarNegativacao(AJson: TJsonObject);
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
        JsonDados.Add('negativacao').Value.AsString := 'True';
        JsonDados.Add('quantidade_dias_negativacao').Value.AsInteger :=
          ATitulo.DiasDeNegativacao;
        JsonPair := TJsonPair.Create(AJson, 'negativacao');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('negativacao').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GeraDadoBoleto(AJson: TJsonObject);
var
  JsonDados: TJsonObject;
  JsonPair: TJsonPair;
begin
  JsonDados := TJsonObject.Create;
  try
    JsonDados.Add('descricao_instrumento_cobranca').Value.AsString := 'boleto';
    JsonDados.Add('tipo_boleto').Value.AsString := 'a vista';
    JsonDados.Add('codigo_carteira').Value.AsInteger :=
      StrToIntDef(ATitulo.carteira, 0);
    JsonDados.Add('valor_total_titulo').Value.AsString :=
      IntToStrZero(round(ATitulo.ValorDocumento * 100), 17);
    JsonDados.Add('codigo_especie').Value.AsString := CodigoEspeciaDoc;
    JsonDados.Add('valor_abatimento').Value.AsString :=
      IntToStrZero(round(ATitulo.ValorAbatimento * 100), 17);
    JsonDados.Add('data_emissao').Value.AsString :=
      FormatDateBr(ATitulo.DataDocumento, 'yyyy-mm-dd');
    if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
      JsonDados.Add('indicador_pagamento_parcial').Value.AsString := 'True'
    else
      JsonDados.Add('indicador_pagamento_parcial').Value.AsString := 'False';
    JsonDados.Add('quantidade_maximo_parcial').Value.AsInteger := 0;
    JsonDados.Add('desconto_expresso').Value.AsString := 'False';

    GerarPagador(JsonDados);
    GerarDadosIndividuaisBoleto(JsonDados);
    GerarMulta(JsonDados);
    GerarJuros(JsonDados);
    GerarDesconto(JsonDados);
    GerarRecebimentoDivergente(JsonDados);
    if ATitulo.Instrucao1 <> '' then
      GerarInstruCaoCobranca(JsonDados);

    if ATitulo.DiasDeProtesto > 0 then
      GerarProtesto(JsonDados);
    if (ATitulo.DiasDeNegativacao > 0) then
      GerarNegativacao(JsonDados);

    JsonPair := TJsonPair.Create(AJson, 'dado_boleto');
    try
      JsonPair.Value.AsObject := JsonDados;
      AJson.Add('dado_boleto').Assign(JsonPair);
    finally
      JsonPair.Free;
    end;
  finally
    JsonDados.Free;
  end;
end;

procedure TBoletoW_Itau_API.GerarData(AJson: TJsonObject);
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
        if OAuth.Ambiente = taHomologacao then
          JsonDados.Add('etapa_processo_boleto').Value.AsString := 'validacao'
        else
          JsonDados.Add('etapa_processo_boleto').Value.AsString := 'efetivacao';
        JsonDados.Add('codigo_canal_operacao').Value.AsString := 'API';
        GeraIdBeneficiario(JsonDados);
        GeraDadoBoleto(JsonDados);
        JsonPair := TJsonPair.Create(AJson, 'data');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('data').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.RequisicaoJson;
var
  Data: string;
  Json: TJsonObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      GerarData(Json);
      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;
end;


procedure TBoletoW_Itau_API.RequisicaoAltera;
var
  Data: string;
  Json: TJsonObject;
begin
  if Assigned(ATitulo) then
  begin

    Json := TJsonObject.Create;
    try

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3: // RemessaConcederAbatimento
          begin
            if (ATitulo.ValorAbatimento > 0) then
              Json.Add('valor_abatimento').Value.AsNumber :=  ATitulo.ValorAbatimento;
          end;
        4: // RemessaCancelarAbatimento
          begin
            Json.Add('valor_abatimento').Value.AsString := '0';
          end;
        5: //RemessaConcederDesconto
          begin
            AtribuirDesconto(Json);
          end;
        7: //RemessaAlterarVencimento
          begin
              Json.Add('data_vencimento').Value.AsString := FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD');
          end;
        9:  //RemessaProtestar
          begin
            AlterarProtesto(Json);
          end;
        10:  //RemessaSustarProtesto
          begin
            AlterarProtesto(Json);
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            AlterarProtesto(Json);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            if (ATitulo.SeuNumero <> '') then
              Json.Add('texto_seu_numero').Value.AsString := PadLeft(ATitulo.SeuNumero,10,'0')
            else
              raise Exception.Create(ACBrStr('Seu número é a identificação do boleto que poderá ' + sLineBreak +
                                 ' ter letras e números e OBRIGATÓRIAMENTE 10 posições' + sLineBreak));
          end;

        37: //RemessaCobrarJurosMora
          begin
            AtribuirJuros(Json);
          end;
        50:  //RemessaAlterarMulta
          begin
             AtribuirMulta(Json);
          end;
        51:  //RemessaDispensarMulta
          begin
             AtribuirMulta(Json);
          end;
        52: //RemessaAlterarDesconto
          begin
            AtribuirDesconto(Json);
          end;
        53: //toRemessaNaoConcederDesconto
          begin
            AtribuirDesconto(Json);
          end;
        55:  //toRemessaAlterarPrazoLimiteRecebimento
          begin
            if (ATitulo.DataLimitePagto > 0) then
              Json.Add('data_limite_pagamento').Value.AsString := FormatDateBr(ATitulo.DataLimitePagto, 'YYYY-MM-DD');
          end;
      end;

      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
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
  JsonDados: TJsonObject;
  JsonPair: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDados := TJsonObject.Create;
      try
        JsonDados.Add('nome_pessoa').Value.AsString :=
          ATitulo.Sacado.NomeSacado;
        GerarTipoPessoaPg(JsonDados);

        JsonPair := TJsonPair.Create(AJson, 'pessoa');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('pessoa').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarTipoPessoaPg(AJson: TJsonObject);
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
        if Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) < 12 then
        begin
          JsonDados.Add('codigo_tipo_pessoa').Value.AsString := 'F';
          JsonDados.Add('numero_cadastro_pessoa_fisica').Value.AsString :=
            OnlyNumber(ATitulo.Sacado.CNPJCPF);
        end
        else
        begin
          JsonDados.Add('codigo_tipo_pessoa').Value.AsString := 'J';
          JsonDados.Add('numero_cadastro_nacional_pessoa_juridica')
            .Value.AsString := OnlyNumber(ATitulo.Sacado.CNPJCPF);
        end;
        JsonPair := TJsonPair.Create(AJson, 'tipo_pessoa');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('tipo_pessoa').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarEnderecoPg(AJson: TJsonObject);
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
        JsonDados.Add('nome_logradouro').Value.AsString :=
          ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero;
        JsonDados.Add('nome_bairro').Value.AsString := ATitulo.Sacado.Bairro;
        JsonDados.Add('nome_cidade').Value.AsString := ATitulo.Sacado.Cidade;
        JsonDados.Add('sigla_UF').Value.AsString := ATitulo.Sacado.UF;
        JsonDados.Add('numero_CEP').Value.AsString := ATitulo.Sacado.CEP;
        JsonPair := TJsonPair.Create(AJson, 'endereco');
        try
          JsonPair.Value.AsObject := JsonDados;
          AJson.Add('endereco').Assign(JsonPair);
        finally
          JsonPair.Free;
        end;
      finally
        JsonDados.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDadosPagador := TJsonObject.Create;
      try
        GerarPessoaPg(JsonDadosPagador);
        GerarEnderecoPg(JsonDadosPagador);
        JsonPairPagador := TJsonPair.Create(AJson, 'pagador');
        try
          JsonPairPagador.Value.AsObject := JsonDadosPagador;
          AJson.Add('pagador').Assign(JsonPairPagador);
        finally
          JsonPairPagador.Free;
        end;
      finally
        JsonDadosPagador.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJsonPair;
  CodigoMora: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonJuros := TJsonObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if ATitulo.CodigoMora = '' then
          begin
            CodigoMora := '90';
            case ATitulo.CodigoMoraJuros of
              cjValorDia:
                CodigoMora := '93';
              cjTaxaDiaria:
                CodigoMora := '91';
              cjTaxaMensal:
                CodigoMora := '90';
              cjIsento:
                CodigoMora := '00';
            end; // cjValorDia, cjTaxaMensal, cjIsento, cjValorMensal, cjTaxaDiaria
          end;
          JsonJuros.Add('codigo_tipo_juros').Value.AsString := CodigoMora;
          JsonJuros.Add('quantidade_dias_juros').Value.AsInteger :=
            trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento);
          if CodigoMora = '93' then
          begin
            JsonJuros.Add('valor_juros').Value.AsString :=
              IntToStrZero(round(ATitulo.ValorMoraJuros * 100), 17)
          end
          else if (CodigoMora = '91') or (CodigoMora = '90') or
            (CodigoMora = '92') then
            JsonJuros.Add('percentual_juros').Value.AsString :=
              IntToStrZero(round(ATitulo.ValorMoraJuros * 100000), 12);

          JsonPairJuros := TJsonPair.Create(AJson, 'juros');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('juros').Assign(JsonPairJuros);
          finally
            JsonPairJuros.Free;
          end;
        end;
      finally
        JsonJuros.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Itau_API.GerarMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJsonObject.Create;
      try
        if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
        begin
          if ATitulo.MultaValorFixo then
            ACodMulta := '01'
          else
            ACodMulta := '02';
        end
        else
          ACodMulta := '03';

        JsonMulta.Add('codigo_tipo_multa').Value.AsString := ACodMulta;
        if (ATitulo.DataMulta > 0) then
        begin
          if ACodMulta = '01' then
            JsonMulta.Add('valor_multa').Value.AsString :=
              IntToStrZero(round(ATitulo.PercentualMulta * 100), 17)
          else if ACodMulta = '02' then
            JsonMulta.Add('percentual_multa').Value.AsString :=
              IntToStrZero(round(ATitulo.PercentualMulta * 100000), 12);
          JsonMulta.Add('quantidade_dias_multa').Value.AsInteger :=
            trunc(ATitulo.DataMulta - ATitulo.Vencimento);
        end;

        JsonPairMulta := TJsonPair.Create(AJson, 'multa');
        try
          JsonPairMulta.Value.AsObject := JsonMulta;
          AJson.Add('multa').Assign(JsonPairMulta);
        finally
          JsonPairMulta.Free;
        end;
      finally
        JsonMulta.Free;
      end;
    end;
  end;
end;

function TBoletoW_Itau_API.TipoDescontoToString(const AValue
  : TACBrTipoDesconto): string;
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
  JsonDesconto: TJsonObject;
  JsonPairDesconto: TJsonPair;
  JsonArrGrupoDesconto: TJsonArray;

begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDesconto := TJsonObject.Create;
        JsonArrGrupoDesconto := TJsonArray.Create;
        try
          JsonDesconto.Add('codigo_tipo_desconto').Value.AsString :=
            TipoDescontoToString(TipoDesconto);
          if DataDesconto > 0 then
          begin
            JsonDesconto.Add('data_desconto').Value.AsString :=
              FormatDateTime('yyyy-mm-dd', DataDesconto);
            if ((Integer(TipoDesconto)) in [1, 3, 4]) then
              JsonDesconto.Add('valor_desconto').Value.AsString :=
                IntToStrZero(round(ValorDesconto * 100), 17)
            else
              JsonDesconto.Add('percentual_desconto').Value.AsString :=
                IntToStrZero(round(ValorDesconto * 100000), 12);
          end;
          JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;

          // Desconto2
          if ValorDesconto2 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('codigo_tipo_desconto').Value.AsString :=
              TipoDescontoToString(TipoDesconto2);
            if DataDesconto2 > 0 then
            begin
              JsonDesconto.Add('data_desconto').Value.AsString :=
                FormatDateTime('yyyy-mm-dd', DataDesconto2);
              if ((Integer(TipoDesconto2)) in [1, 3, 4]) then
                JsonDesconto.Add('valor_desconto').Value.AsString :=
                  IntToStrZero(round(ValorDesconto2 * 100), 17)
              else
                JsonDesconto.Add('percentual_desconto').Value.AsString :=
                  IntToStrZero(round(ValorDesconto2 * 100000), 12);
            end;
            JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'desconto ');
          try
            JsonPairDesconto.Value.AsArray := JsonArrGrupoDesconto;
            AJson.Add('desconto ').Assign(JsonPairDesconto);
          finally
            JsonPairDesconto.Free;
          end;

        finally
          JsonArrGrupoDesconto.Free;
          JsonDesconto.Free;
        end;
      end;

    end;
  end;

end;


procedure TBoletoW_Itau_API.AtribuirDesconto(AJson: TJsonObject);
var
  JsonAtribuirDesconto : TJsonObject;
  JsonPairAtribuirDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirDesconto := TJsonObject.Create;
      try

        case ATitulo.OcorrenciaOriginal.Tipo of
          toRemessaCancelarDesconto,toRemessaNaoConcederDesconto:
          begin
             JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '00'; // Quando não houver condição de desconto ou efetuar exclusão de desconto.
          end
        else
          begin
            if (ATitulo.ValorDesconto > 0) then
            begin
              case ATitulo.TipoDesconto of
                tdNaoConcederDesconto                       : JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '00'; // Quando não houver condição de desconto ou efetuar exclusão de desconto.
                tdValorFixoAteDataInformada                 : JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '1';  // Concede desconto de valor fixo, se o título for pago até a data estipulada
                tdPercentualAteDataInformada                : JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '2';  // Concede desconto percentual fixo, se o título for pago até a data estipulada
                tdValorAntecipacaoDiaCorrido                : JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '91'; // Até a data de vencimento, concede valor de desconto por dia antecipado de pagamento.
                tdPercentualSobreValorNominalDiaCorrido     : JsonAtribuirDesconto.Add('codigo_tipo_desconto').Value.AsString := '90'; // Até a data de vencimento, concede desconto percentual por dia antecipado de pagamento.
              else
                raise Exception.Create(ACBrStr('TipoDesconto não é válido para este banco' + sLineBreak));
              end
            end
            else
            raise Exception.Create(ACBrStr('Valor do desconto é obrigatório' + sLineBreak));
            if Integer(ATitulo.TipoDesconto) > 0 then
               AtribuirDesconto1(JsonAtribuirDesconto);
          end;

        end;


        JsonPairAtribuirDesconto := TJsonPair.Create(AJson, 'desconto');
        try
          JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
          AJson.Add('desconto').Assign(JsonPairAtribuirDesconto);
        finally
          JsonPairAtribuirDesconto.Free;
        end;


      finally
        JsonAtribuirDesconto.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_Itau_API.AtribuirDesconto1(AJson: TJsonObject);
var
  JsonDesconto : TJsonObject;
  JsonArrGrupoDesconto : TJsonArray;
  JsonPairDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDesconto := TJSONObject.Create;
        JsonArrGrupoDesconto := TJsonArray.Create;
        try
          if ATitulo.DataDesconto > 0 then
          begin
            if (ATitulo.TipoDesconto = tdValorFixoAteDataInformada) or (ATitulo.TipoDesconto = tdPercentualAteDataInformada) then
               JsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto,ATitulo.Vencimento);
            if (ATitulo.ValorDesconto > 0) then
            begin
              case ATitulo.TipoDesconto of
                tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                    JsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto);
                tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                    JsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto);
              end;
            end
            else
              raise Exception.Create(ACBrStr('Obrigatório informar o valor do desconto!'));
            JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;
          end;

          //Desconto2
          if ATitulo.DataDesconto2 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto2,ATitulo.Vencimento);
            if (ATitulo.ValorDesconto2 > 0) then
            begin
              case ATitulo.TipoDesconto2 of
                tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                    JsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto2);
                tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                    JsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto2);
              end;
            end;
            JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;
          end;

          //Desconto3
          if ATitulo.DataDesconto3 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('quantidade_dias_desconto').Value.AsInteger := DaysBetween(ATitulo.DataDesconto3,ATitulo.Vencimento);
            if (ATitulo.ValorDesconto2 > 0) then
            begin
              case ATitulo.TipoDesconto2 of
                tdValorFixoAteDataInformada, tdValorAntecipacaoDiaCorrido:
                    JsonDesconto.Add('valor_desconto').Value.AsString := CurrToStrItau(ATitulo.ValorDesconto3);
                tdPercentualAteDataInformada,tdPercentualSobreValorNominalDiaCorrido:
                    JsonDesconto.Add('percentual_desconto').Value.AsString := PercToStrItau(ATitulo.ValorDesconto3);
              end;
            end;
            JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'descontos');
          try
            JsonPairDesconto.Value.AsArray := JsonArrGrupoDesconto;
            AJson.Add('descontos').Assign(JsonPairDesconto);
          finally
            JsonPairDesconto.Free;
          end;

        finally
          JsonArrGrupoDesconto.Free;
          JsonDesconto.Free;
        end;
      end;

    end;
  end;

end;




//procedure TBoletoW_Itau_API.AlteracaoDesconto(AJson: TJsonObject);
//var
//  JsonAtribuirDesconto: TJsonObject;
//  JsonPairAtribuirDesconto: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirDesconto := TJsonObject.Create;
//      try
//        if (ATitulo.ValorDesconto > 0) then
//        begin
//          JsonAtribuirDesconto.Add('tipoPrimeiroDesconto').Value.AsInteger :=
//            Integer(ATitulo.TipoDesconto);
//          case Integer(ATitulo.TipoDesconto) of
//            1:
//              JsonAtribuirDesconto.Add('novoValorPrimeiroDesconto')
//                .Value.AsNumber := ATitulo.ValorDesconto;
//            2:
//              JsonAtribuirDesconto.Add('novoPercentualPrimeiroDesconto')
//                .Value.AsNumber := ATitulo.ValorDesconto;
//          end;
//          JsonAtribuirDesconto.Add('novaDataLimitePrimeiroDesconto')
//            .Value.AsString := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
//        end;
//        if (ATitulo.ValorDesconto2 > 0) then
//        begin
//          JsonAtribuirDesconto.Add('tipoSegundoDesconto').Value.AsInteger :=
//            Integer(ATitulo.TipoDesconto2);
//          case Integer(ATitulo.TipoDesconto2) of
//            1:
//              JsonAtribuirDesconto.Add('novoValorSegundoDesconto')
//                .Value.AsNumber := ATitulo.ValorDesconto2;
//            2:
//              JsonAtribuirDesconto.Add('novoPercentualSegundoDesconto')
//                .Value.AsNumber := ATitulo.ValorDesconto2;
//          end;
//          JsonAtribuirDesconto.Add('novaDataLimiteSegundoDesconto')
//            .Value.AsString := FormatDateBr(ATitulo.DataDesconto2,
//            'DD.MM.YYYY');
//        end;
//
//        if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
//        begin
//          JsonPairAtribuirDesconto := TJsonPair.Create(AJson,
//            'alteracaoDesconto');
//          try
//            JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
//            AJson.Add('alteracaoDesconto').Assign(JsonPairAtribuirDesconto);
//          finally
//            JsonPairAtribuirDesconto.Free;
//          end;
//
//        end;
//
//      finally
//        JsonAtribuirDesconto.Free;
//      end;
//    end;
//
//  end;
//
//end;

//procedure TBoletoW_Itau_API.AlteracaoDataDesconto(AJson: TJsonObject);
//var
//  JsonAtribuirDesconto: TJsonObject;
//  JsonPairAtribuirDesconto: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirDesconto := TJsonObject.Create;
//      try
//        if (ATitulo.DataDesconto > 0) then
//        begin
//          JsonAtribuirDesconto.Add('novaDataLimitePrimeiroDesconto')
//            .Value.AsString := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
//        end;
//        if (ATitulo.DataDesconto2 > 0) then
//        begin
//          JsonAtribuirDesconto.Add('novaDataLimiteSegundoDesconto')
//            .Value.AsString := FormatDateBr(ATitulo.DataDesconto2,
//            'DD.MM.YYYY');
//        end;
//
//        if (ATitulo.DataDesconto > 0) or (ATitulo.DataDesconto2 > 0) then
//        begin
//          JsonPairAtribuirDesconto := TJsonPair.Create(AJson,
//            'alteracaoDataDesconto');
//          try
//            JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
//            AJson.Add('alteracaoDataDesconto').Assign(JsonPairAtribuirDesconto);
//          finally
//            JsonPairAtribuirDesconto.Free;
//          end;
//
//        end;
//
//      finally
//        JsonAtribuirDesconto.Free;
//      end;
//    end;
//
//  end;
//
//end;

procedure TBoletoW_Itau_API.AlterarProtesto(AJson: TJsonObject);
var
  JsonAtribuirProtesto: TJsonObject;
  JsonPairAtribuirProtesto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirProtesto := TJsonObject.Create;
      try
        case ATitulo.OcorrenciaOriginal.Tipo of
          toRemessaProtestar :
            begin
              if (ATitulo.DiasDeProtesto > 0) then
              begin
                JsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger     := 1;    //   protestar
                JsonAtribuirProtesto.Add('quantidade_dias_protesto').Value.AsInteger := ATitulo.DiasDeProtesto;
              end
              else
              raise Exception.Create(ACBrStr('DiasDeProtesto obrigatório !'));
            end;
          toRemessaSustarProtesto:
            JsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger     := 4; // nao protestar
          toRemessaCancelarInstrucaoProtesto:
            JsonAtribuirProtesto.Add('codigo_tipo_protesto').Value.AsInteger     := 9; // cancelar protesto
        end;


          JsonPairAtribuirProtesto := TJsonPair.Create(AJson, 'protesto');
          try
            JsonPairAtribuirProtesto.Value.AsObject := JsonAtribuirProtesto;
            AJson.Add('protesto').Assign(JsonPairAtribuirProtesto);
          finally
            JsonPairAtribuirProtesto.Free;
          end;
      finally
        JsonAtribuirProtesto.Free;
      end;
    end;
  end;

end;

//procedure TBoletoW_Itau_API.AtribuirAbatimento(AJson: TJsonObject);
//var
//  JsonAtribuirAbatimento: TJsonObject;
//  JsonPairAtribuirAbatimento: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirAbatimento := TJsonObject.Create;
//      try
//        if (ATitulo.ValorAbatimento > 0) then
//        begin
//          JsonAtribuirAbatimento.Add('valor_abatimento').Value.AsNumber :=
//            ATitulo.ValorAbatimento;
//          JsonPairAtribuirAbatimento := TJsonPair.Create(AJson, 'abatimento');
//          try
//            JsonPairAtribuirAbatimento.Value.AsObject := JsonAtribuirAbatimento;
//            AJson.Add('abatimento').Assign(JsonPairAtribuirAbatimento);
//          finally
//            JsonPairAtribuirAbatimento.Free;
//          end;
//
//        end;
//
//      finally
//        JsonAtribuirAbatimento.Free;
//      end;
//    end;
//
//  end;
//
//end;

//procedure TBoletoW_Itau_API.AlteracaoAbatimento(AJson: TJsonObject);
//var
//  JsonAtribuirAbatimento: TJsonObject;
//  JsonPairAtribuirAbatimento: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirAbatimento := TJsonObject.Create;
//      try
//        if (ATitulo.ValorAbatimento > 0) then
//        begin
//          JsonAtribuirAbatimento.Add('novoValorAbatimento').Value.AsNumber :=
//            ATitulo.ValorAbatimento;
//
//          JsonPairAtribuirAbatimento := TJsonPair.Create(AJson,
//            'alteracaoAbatimento');
//          try
//            JsonPairAtribuirAbatimento.Value.AsObject := JsonAtribuirAbatimento;
//            AJson.Add('alteracaoAbatimento').Assign(JsonPairAtribuirAbatimento);
//          finally
//            JsonPairAtribuirAbatimento.Free;
//          end;
//
//        end;
//      finally
//        JsonAtribuirAbatimento.Free;
//      end;
//    end;
//
//  end;
//
//end;

procedure TBoletoW_Itau_API.AtribuirJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJsonPair;
  CodigoMora: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonJuros := TJsonObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if ATitulo.CodigoMora = '' then
          begin
            CodigoMora := '90';
            case ATitulo.CodigoMoraJuros of
              cjValorDia:
                CodigoMora := '93';
              cjTaxaMensal:
                CodigoMora := '90';
              else
              raise Exception.Create(ACBrStr('CodigoMoraJuros permitido somente ' + sLineBreak +
                                 ' cjValorDia ou cjTaxaMensal !' + sLineBreak));

            end;
          end;
          JsonJuros.Add('codigo_tipo_juros').Value.AsString := CodigoMora;
          JsonJuros.Add('quantidade_dias_juros').Value.AsInteger :=
            trunc(ATitulo.DataMoraJuros - ATitulo.Vencimento);
          if CodigoMora = '93' then
          begin
            JsonJuros.Add('valor_juros').Value.AsString := CurrToStrItau(ATitulo.ValorMoraJuros) ;
          end
          else if (CodigoMora = '90') then
            JsonJuros.Add('percentual_juros').Value.AsString := PercToStrItau(ATitulo.ValorMoraJuros);

          JsonPairJuros := TJsonPair.Create(AJson, 'juros');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('juros').Assign(JsonPairJuros);
          finally
            JsonPairJuros.Free;
          end;
        end;
      finally
        JsonJuros.Free;
      end;
    end;
  end;
end;


procedure TBoletoW_Itau_API.AtribuirMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: String;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJsonObject.Create;
      try
        case ATitulo.OcorrenciaOriginal.Tipo of
        toRemessaAlterarMulta :
          begin
            if (ATitulo.PercentualMulta > 0) and (ATitulo.DataMulta > 0) then
            begin
              if ATitulo.MultaValorFixo then
                ACodMulta := '01'
              else
                ACodMulta := '02';
            end
            else
              raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta ou PercentualMulta não foi informada ! ' + sLineBreak));

            JsonMulta.Add('codigo_tipo_multa').Value.AsString := ACodMulta;
            if (ATitulo.DataMulta > 0) then
            begin
              if ACodMulta = '01' then
                JsonMulta.Add('valor_multa').Value.AsString := CurrToStrItau(ATitulo.PercentualMulta)
              else if ACodMulta = '02' then
                JsonMulta.Add('percentual_multa').Value.AsString := PercToStrItau(ATitulo.PercentualMulta);
              JsonMulta.Add('quantidade_dias_multa').Value.AsInteger :=
                trunc(ATitulo.DataMulta - ATitulo.Vencimento);
            end
            else
              raise Exception.Create(ACBrStr('A propriedade do titulo DataMulta não foi informada ! '));

          end;
        toRemessaDispensarMulta :
          JsonMulta.Add('codigo_tipo_multa').Value.AsString := '03'; // dispensar multa
        end;

        JsonPairMulta := TJsonPair.Create(AJson, 'multa');
        try
          JsonPairMulta.Value.AsObject := JsonMulta;
          AJson.Add('multa').Assign(JsonPairMulta);
        finally
          JsonPairMulta.Free;
        end;
      finally
        JsonMulta.Free;
      end;
    end;
  end;
end;

//procedure TBoletoW_Itau_API.AtribuirNegativacao(AJson: TJsonObject);
//var
//  JsonAtribuirNegativacao: TJsonObject;
//  JsonPairAtribuirNegativacao: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirNegativacao := TJsonObject.Create;
//      try
//        if (ATitulo.DiasDeNegativacao > 0) then
//        begin
//          JsonAtribuirNegativacao.Add('quantidadeDiasNegativacao')
//            .Value.AsInteger := ATitulo.DiasDeNegativacao;
//          JsonAtribuirNegativacao.Add('tipoNegativacao').Value.AsInteger := 1;
//
//          JsonPairAtribuirNegativacao := TJsonPair.Create(AJson, 'negativacao');
//          try
//            JsonPairAtribuirNegativacao.Value.AsObject :=
//              JsonAtribuirNegativacao;
//            AJson.Add('negativacao').Assign(JsonPairAtribuirNegativacao);
//          finally
//            JsonPairAtribuirNegativacao.Free;
//          end;
//
//        end;
//      finally
//        JsonAtribuirNegativacao.Free;
//      end;
//    end;
//
//  end;
//
//end;

//procedure TBoletoW_Itau_API.AlteracaoSeuNumero(AJson: TJsonObject);
//var
//  JsonAtribuirSeuNumero: TJsonObject;
//  JsonPairAtribuirSeuNumero: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirSeuNumero := TJsonObject.Create;
//      try
//        if (ATitulo.SeuNumero <> '') then
//        begin
//          JsonAtribuirSeuNumero.Add('texto_seu_numero').Value.AsString :=  ATitulo.SeuNumero;
//
//          JsonPairAtribuirSeuNumero := TJsonPair.Create(AJson,
//            'texto_seu_numero');
//          try
//            JsonPairAtribuirSeuNumero.Value.AsObject := JsonAtribuirSeuNumero;
//            AJson.Add('alteracaoSeuNumero').Assign(JsonPairAtribuirSeuNumero);
//          finally
//            JsonPairAtribuirSeuNumero.Free;
//          end;
//
//        end;
//      finally
//        JsonAtribuirSeuNumero.Free;
//      end;
//    end;
//
//  end;
//
//end;

//procedure TBoletoW_Itau_API.AlteracaoEnderecoPagador(AJson: TJsonObject);
//var
//  JsonAtribuirEndereco: TJsonObject;
//  JsonPairAtribuirEndereco: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirEndereco := TJsonObject.Create;
//      try
//        if (ATitulo.SeuNumero <> '') then
//        begin
//          JsonAtribuirEndereco.Add('enderecoPagador').Value.AsString :=
//            ATitulo.Sacado.Logradouro;
//          JsonAtribuirEndereco.Add('bairroPagador').Value.AsString :=
//            ATitulo.Sacado.Bairro;
//          JsonAtribuirEndereco.Add('cidadePagador').Value.AsString :=
//            ATitulo.Sacado.Cidade;
//          JsonAtribuirEndereco.Add('UFPagador').Value.AsString :=
//            ATitulo.Sacado.UF;
//          JsonAtribuirEndereco.Add('CEPPagador').Value.AsString :=
//            ATitulo.Sacado.CEP;
//
//          JsonPairAtribuirEndereco := TJsonPair.Create(AJson,
//            'alteracaoEndereco');
//          try
//            JsonPairAtribuirEndereco.Value.AsObject := JsonAtribuirEndereco;
//            AJson.Add('alteracaoEndereco').Assign(JsonPairAtribuirEndereco);
//          finally
//            JsonPairAtribuirEndereco.Free;
//          end;
//
//        end;
//      finally
//        JsonAtribuirEndereco.Free;
//      end;
//    end;
//
//  end;
//
//end;

//procedure TBoletoW_Itau_API.AlteracaoPrazo(AJson: TJsonObject);
//var
//  JsonAtribuirAlteracaoPrazo: TJsonObject;
//  JsonPairAtribuirAlteracaoPrazo: TJsonPair;
//
//begin
//  if Assigned(ATitulo) then
//  begin
//    if Assigned(AJson) then
//    begin
//      JsonAtribuirAlteracaoPrazo := TJsonObject.Create;
//      try
//        if (ATitulo.SeuNumero <> '') then
//        begin
//          JsonAtribuirAlteracaoPrazo.Add('quantidadeDiasAceite').Value.AsInteger
//            := DaysBetween(ATitulo.Vencimento, ATitulo.DataLimitePagto);
//
//          JsonPairAtribuirAlteracaoPrazo :=
//            TJsonPair.Create(AJson, 'alteracaoPrazo');
//          try
//            JsonPairAtribuirAlteracaoPrazo.Value.AsObject :=
//              JsonAtribuirAlteracaoPrazo;
//            AJson.Add('alteracaoPrazo').Assign(JsonPairAtribuirAlteracaoPrazo);
//          finally
//            JsonPairAtribuirAlteracaoPrazo.Free;
//          end;
//
//        end;
//      finally
//        JsonAtribuirAlteracaoPrazo.Free;
//      end;
//    end;
//
//  end;
//
//end;

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
  // FProtocol := Protocolo;
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
