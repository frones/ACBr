{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  José M S Junior                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBoletoW_Itau;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  ACBrBoleto,
  Jsons,
  ACBrUtil.Base,
  ACBrBoletoWS.Rest;

type

  { TBoletoW_Itau }
  TBoletoW_Itau = class(TBoletoWSREST)
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;

    procedure DefinirKeyUser;
    procedure DefinirIdentificador;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;

    procedure RequisicaoJson;
    procedure GerarBeneficiario(AJson: TJsonObject);
    procedure GerarDebito(AJson: TJsonObject);
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarEmailPagador(AJSon: TJsonObject);
    procedure GerarSacadorAvalista(AJson: TJsonObject);
    procedure GerarMoeda(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GerarRecebimentoDivergente(AJson: TJsonObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL = 'https://gerador-boletos.itau.com.br/router-gateway-app/public/codigo_barras/registro';
  C_URL_HOM = 'https://gerador-boletos.itau.com.br/router-gateway-app/public/codigo_barras/registro';
  C_URL_OAUTH_HOM = 'https://oauth.itau.com.br/identity/connect/token';
  C_URL_OAUTH_PROD = 'https://oauth.itau.com.br/identity/connect/token';
  C_ACCEPT = 'application/vnd.itau';

implementation

uses
  strutils, ACBrUtil.Strings;

{ TBoletoW_Itau }

procedure TBoletoW_Itau.DefinirURL;
begin
  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,
    C_URL, C_URL_HOM);

end;

procedure TBoletoW_Itau.DefinirContentType;
begin
  FPContentType := '';
end;


procedure TBoletoW_Itau.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
  DefinirIdentificador;

end;

procedure TBoletoW_Itau.GerarDados;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui: RequisicaoJson;
        else
          raise EACBrBoletoWSException.Create(ClassName + Format(
            S_OPERACAO_NAO_IMPLEMENTADO, [
            TipoOperacaoToStr(
            Boleto.Configuracoes.WebService.Operacao)]));
      end;

    end;
end;

procedure TBoletoW_Itau.DefinirAuthorization;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Itau.GerarTokenAutenticacao: string;
begin
  Result := inherited GerarTokenAutenticacao;

end;

procedure TBoletoW_Itau.DefinirKeyUser;
begin
    FPKeyUser := 'itau-chave: ' + Boleto.Cedente.CedenteWS.KeyUser;
end;

procedure TBoletoW_Itau.DefinirIdentificador;
begin
  if Assigned(ATitulo) then
    FPIdentificador := 'identificador: ' + OnlyNumber(
      ATitulo.ACBrBoleto.Cedente.CNPJCPF);
end;

procedure TBoletoW_Itau.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Itau.ValidaAmbiente: Integer;
begin
  if Boleto.Configuracoes.WebService.Ambiente = taProducao then
    Result := 2
  else
    Result := 1;
end;

procedure TBoletoW_Itau.RequisicaoJson;
var
  Data, AEspecieDoc: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if AnsiSameText(EspecieDoc,'DM') then
         AEspecieDoc:= '01'
      else if AnsiSameText(EspecieDoc, 'NP') then
         AEspecieDoc:= '02'
      else if AnsiSameText(EspecieDoc, 'NS') then
         AEspecieDoc:= '03'
      else if AnsiSameText(EspecieDoc, 'CS') then
         AEspecieDoc:= '04'
      else if AnsiSameText(EspecieDoc, 'REC') then
         AEspecieDoc:= '05'
      else if AnsiSameText(EspecieDoc, 'LC') then
         AEspecieDoc:= '09'
      else if AnsiSameText(EspecieDoc, 'DS') then
         AEspecieDoc:= '08'
      else if AnsiSameText(EspecieDoc, 'ND') then
         AEspecieDoc:= '13'
      else if AnsiSameText(EspecieDoc, 'PS') then
         AEspecieDoc:= '17'
      else if AnsiSameText(EspecieDoc, 'BP') then
         AEspecieDoc:= '18'
      else
         AEspecieDoc:= '99';

      Json := TJsonObject.Create;
      try
        Json.Add('tipo_ambiente').Value.AsInteger := ValidaAmbiente;
        Json.Add('tipo_registro').Value.AsInteger := 1;
        Json.Add('tipo_cobranca').Value.AsInteger := 1;
        Json.Add('tipo_produto').Value.AsString := '00006';
        Json.Add('subproduto').Value.AsString := '00008';         //Valores fixo conforme estabelecido no manual

        GerarBeneficiario(Json);
        GerarDebito(Json);

        Json.Add('identificador_titulo_empresa').Value.AsString := NumeroDocumento;
        Json.Add('uso_banco').Value.AsString := '';
        Json.Add('titulo_aceite').Value.AsString := 'S';

        GerarPagador(Json);
        GerarSacadorAvalista(Json);

        Json.Add('tipo_carteira_titulo').Value.AsInteger := StrToInt(Carteira);
        GerarMoeda(Json);

        Json.Add('nosso_numero').Value.AsString := NossoNumero;
        Json.Add('digito_verificador_nosso_numero').Value.AsString :=
          ACBrBoleto.Banco.CalcularDigitoVerificador(ATitulo);
        //Json.Add('codigo_barras').Value.AsString := '';
        Json.Add('data_vencimento').Value.AsString :=
          FormatDateTime('yyyy-mm-dd', Vencimento);
        Json.Add('valor_cobrado').Value.AsString :=
          IntToStrZero(round(ValorDocumento * 100), 17);
        Json.Add('seu_numero').Value.AsString := SeuNumero;
        Json.Add('especie').Value.AsString := AEspecieDoc;
        Json.Add('data_emissao').Value.AsString :=
          FormatDateTime('yyyy-mm-dd', DataDocumento);
        if DataLimitePagto > 0 then
          Json.Add('data_limite_pagamento').Value.AsString := FormatDateTime('yyyy-mm-dd', DataLimitePagto);

        Json.Add('tipo_pagamento').Value.AsInteger := 3;
        Json.Add('indicador_pagamento_parcial').Value.AsBoolean := (TipoPagamento <> tpNao_Aceita_Valor_Divergente);
        if TipoPagamento <> tpNao_Aceita_Valor_Divergente then
          Json.Add('quantidade_pagamento_parcial').Value.AsInteger := QtdePagamentoParcial;
        if QtdeParcelas > 0 then
          Json.Add('quantidade_parcelas').Value.AsInteger := QtdeParcelas;
        if Instrucao1 <> '' then
        begin
          Json.Add('instrucao_cobranca_1').Value.AsString := Copy(trim((Instrucao1)), 1, 2);
          if Copy(trim((Instrucao1)), 3, 2) <> '' then
            Json.Add('quantidade_dias_1').Value.AsString := Copy(trim((Instrucao1)), 3, 2);
        end;
        if Instrucao2 <> '' then
        begin
          Json.Add('instrucao_cobranca_2').Value.AsString := Copy(trim((Instrucao2)), 1, 2);
          if Copy(trim((Instrucao2)), 3, 2) <> '' then
            Json.Add('quantidade_dias_2').Value.AsString := Copy(trim((Instrucao2)), 3, 2);
        end;
        if Instrucao3 <> '' then
        begin
          Json.Add('instrucao_cobranca_3').Value.AsString := Copy(trim((Instrucao3)), 1, 2);
          if Copy(trim((Instrucao3)), 3, 2) <> '' then
            Json.Add('quantidade_dias_3').Value.AsString := Copy(trim((Instrucao3)), 3, 2);
        end;
        if ValorAbatimento > 0 then
          Json.Add('valor_abatimento').Value.AsString := IntToStrZero(round(ValorAbatimento * 100), 17);

        GerarJuros(Json);
        GerarMulta(Json);
        GerarDesconto(Json);
        GerarRecebimentoDivergente(Json);

        Data := Json.Stringify;

        FPDadosMsg := Data;
        //WriteToTXT('C:\temp\json.txt', Data, False, False);
      finally
        Json.Free;
      end;

    end;
end;

procedure TBoletoW_Itau.GerarBeneficiario(AJson: TJsonObject);
var
  JsonConta: TJsonObject;
  JsonPairBeneficiario: TJsonPair;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonConta := TJSONObject.Create;
        try
          JsonConta.Add('cpf_cnpj_beneficiario').Value.AsString :=
            OnlyNumber(ACBrBoleto.Cedente.CNPJCPF);
          JsonConta.Add('agencia_beneficiario').Value.AsString :=
            ACBrBoleto.Cedente.Agencia;
          JsonConta.Add('conta_beneficiario').Value.AsString := ACBrUtil.Strings.PadLeft(ACBrBoleto.Cedente.Conta, 7, '0');
          JsonConta.Add('digito_verificador_conta_beneficiario').Value.AsString :=
            ACBrBoleto.Cedente.ContaDigito;


          JsonPairBeneficiario := TJsonPair.Create(AJson, 'beneficiario');
          try
            JsonPairBeneficiario.Value.AsObject := JsonConta;
            AJson.Add('beneficiario').Assign(JsonPairBeneficiario);
          finally
            JsonPairBeneficiario.Free;
          end;
        finally
          JsonConta.Free;
        end;
      end;

    end;

end;

procedure TBoletoW_Itau.GerarDebito(AJson: TJsonObject);
var
  JsonConta: TJsonObject;
  JsonPairDebito: TJsonPair;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Liquidacao.Agencia = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        JsonConta := TJSONObject.Create;
        try
          JsonConta.Add('agencia_debito').Value.AsString := Liquidacao.Agencia;
          JsonConta.Add('conta_debito').Value.AsString := '';
          JsonConta.Add('digito_verificador_conta_debito').Value.AsString := '';

          JsonPairDebito := TJsonPair.Create(AJson, 'debito');
          try
            JsonPairDebito.Value.AsObject := JsonConta;
            AJson.Add('debito').Assign(JsonPairDebito);
          finally
            JsonPairDebito.Free;
          end;
        finally
          JsonConta.Free;
        end;
      end;

    end;

end;

procedure TBoletoW_Itau.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJsonPair;

begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDadosPagador := TJSONObject.Create;

        try
          JsonDadosPagador.Add('cpf_cnpj_pagador').Value.AsString := OnlyNumber(Sacado.CNPJCPF);
          JsonDadosPagador.Add('nome_pagador').Value.AsString := Copy(Sacado.NomeSacado, 1, 30);
          JsonDadosPagador.Add('logradouro_pagador').Value.AsString := Trim(Copy(Sacado.Logradouro + ' ' + Sacado.Numero, 1, 40));
          JsonDadosPagador.Add('bairro_pagador').Value.AsString := Copy(Sacado.Bairro, 1, 15);
          JsonDadosPagador.Add('cidade_pagador').Value.AsString := Copy(Sacado.Cidade, 1, 20);
          JsonDadosPagador.Add('uf_pagador').Value.AsString := Sacado.UF;
          JsonDadosPagador.Add('cep_pagador').Value.AsString := ACBrUtil.Strings.PadLeft(OnlyNumber(Sacado.CEP), 8, '0');
          if (Sacado.Email <> '') then
            GerarEmailPagador(JsonDadosPagador);

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

procedure TBoletoW_Itau.GerarEmailPagador(AJSon: TJsonObject);
var
  JsonMailPagador: TJsonObject;
  JsonArrMailPagador: TJsonArray;
  JsonPairMail: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonMailPagador := TJSONObject.Create;
        JsonArrMailPagador := TJsonArray.Create;
        try
          JsonMailPagador.Add('email_pagador').Value.AsString := Sacado.Email;

          //JsonArrMailPagador.Insert(0).AsObject := JsonMailPagador;
          JsonArrMailPagador.Add.AsObject := JsonMailPagador;

          JsonPairMail := TJsonPair.Create(AJson, 'grupo_email_pagador');
          try
            JsonPairMail.Value.AsArray := JsonArrMailPagador;
            AJson.Add('grupo_email_pagador').Assign(JsonPairMail);
          finally
            JsonPairMail.Free;
          end;

        finally
          JsonArrMailPagador.Free;
          JsonMailPagador.Free;
        end;
      end;

    end;
  end;

end;

procedure TBoletoW_Itau.GerarSacadorAvalista(AJson: TJsonObject);
var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJsonPair;

begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        JsonSacadorAvalista := TJSONObject.Create;

        try
          JsonSacadorAvalista.Add('cpf_cnpj_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.CNPJCPF;
          JsonSacadorAvalista.Add('nome_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.NomeAvalista;
          JsonSacadorAvalista.Add('logradouro_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.Logradouro;
          JsonSacadorAvalista.Add('bairro_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.Bairro;
          JsonSacadorAvalista.Add('cidade_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.Cidade;
          JsonSacadorAvalista.Add('uf_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.UF;
          JsonSacadorAvalista.Add('cep_sacador_avalista').Value.AsString :=
            Sacado.SacadoAvalista.CEP;

          JsonPairSacadorAvalista := TJsonPair.Create(AJson, 'sacador_avalista');
          try
            JsonPairSacadorAvalista.Value.AsObject := JsonSacadorAvalista;
            AJson.Add('sacador_avalista').Assign(JsonPairSacadorAvalista);
          finally
            JsonPairSacadorAvalista.Free;
          end;
        finally
          JsonSacadorAvalista.Free;
        end;
      end;

    end;
end;

procedure TBoletoW_Itau.GerarMoeda(AJson: TJsonObject);
var
  JsonMoeda: TJsonObject;
  JsonPairMoeda: TJsonPair;

begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonMoeda := TJSONObject.Create;

        try
          JsonMoeda.Add('codigo_moeda_cnab').Value.AsString := '09';
          //JsonMoeda.Add('quantidade_moeda').Value.AsString := '';

          JsonPairMoeda := TJsonPair.Create(AJson, 'moeda');
          try
            JsonPairMoeda.Value.AsObject := JsonMoeda;
            AJson.Add('moeda').Assign(JsonPairMoeda);
          finally
            JsonPairMoeda.Free;
          end;
        finally
          JsonMoeda.Free;
        end;
      end;

    end;
end;

procedure TBoletoW_Itau.GerarJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJsonPair;

begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonJuros := TJSONObject.Create;
        try
          if CodigoMora = '' then
          begin
            case CodigoMoraJuros of
              cjValorDia: CodigoMora := '1';
              cjTaxaDiaria: CodigoMora := '2';
              cjTaxaMensal: CodigoMora := '3';
              cjIsento: CodigoMora := '5';
            end;
          end;

          JsonJuros.Add('tipo_juros').Value.AsInteger := StrToInt64Def(CodigoMora, 5);

          if StrToInt64Def(CodigoMora, 5) <> 5 then // se isento (5) não informar o bloco
          begin
            if DataMoraJuros > 0 then
            begin
              JsonJuros.Add('data_juros').Value.AsString :=
                FormatDateTime('yyyy-mm-dd', DataMoraJuros);
              if ((StrToInt64Def(CodigoMora, 5)) in [1, 6]) then
                JsonJuros.Add('valor_juros').Value.AsString :=
                  IntToStrZero(round(ValorMoraJuros * 100), 17)
              else
                JsonJuros.Add('percentual_juros').Value.AsString :=
                  IntToStrZero(round(ValorMoraJuros * 100000), 12);
            end;
          end;

          JsonPairJuros := TJsonPair.Create(AJson, 'juros');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('juros').Assign(JsonPairJuros);
          finally
            JsonPairJuros.Free;
          end;
        finally
          JsonJuros.Free;
        end;
      end;

    end;
end;

procedure TBoletoW_Itau.GerarMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: integer;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonMulta := TJSONObject.Create;

        try
          if PercentualMulta > 0 then
          begin
            if MultaValorFixo then
              ACodMulta := 1
            else
              ACodMulta := 2;
          end
          else
            ACodMulta := 3;

          JsonMulta.Add('tipo_multa').Value.AsInteger := ACodMulta;
          if DataMulta > 0 then
          begin
            JsonMulta.Add('data_multa').Value.AsString := FormatDateTime('yyyy-mm-dd', DataMulta);
            if MultaValorFixo then
              JsonMulta.Add('valor_multa').Value.AsString := IntToStrZero(round(PercentualMulta * 100), 17)
            else
              JsonMulta.Add('percentual_multa').Value.AsString := IntToStrZero(round(PercentualMulta * 100000), 12);

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

procedure TBoletoW_Itau.GerarDesconto(AJson: TJsonObject);
var
  JsonDesconto: TJsonObject;
  JsonArrGrupoDesconto: TJsonArray;
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
          JsonDesconto.Add('tipo_desconto').Value.AsInteger := integer(TipoDesconto);
          if DataDesconto > 0 then
          begin
            JsonDesconto.Add('data_desconto').Value.AsString :=
              FormatDateTime('yyyy-mm-dd', DataDesconto);
            if ((integer(TipoDesconto)) in [1, 3, 4]) then
              JsonDesconto.Add('valor_desconto').Value.AsString :=
                IntToStrZero(round(ValorDesconto * 100), 17)
            else
              JsonDesconto.Add('percentual_desconto').Value.AsString :=
                IntToStrZero(round(ValorDesconto * 100000), 12);
          end;
          JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;

          //Desconto2
          if ValorDesconto2 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('tipo_desconto').Value.AsInteger := integer(TipoDesconto2);
            if DataDesconto > 0 then
            begin
              JsonDesconto.Add('data_desconto').Value.AsString :=
                FormatDateTime('yyyy-mm-dd', DataDesconto2);
              if ((integer(TipoDesconto2)) in [1, 3, 4]) then
                JsonDesconto.Add('valor_desconto').Value.AsString :=
                  IntToStrZero(round(ValorDesconto2 * 100), 17)
              else
                JsonDesconto.Add('percentual_desconto').Value.AsString :=
                  IntToStrZero(round(ValorDesconto2 * 100000), 12);
            end;
            JsonArrGrupoDesconto.Add.AsObject := JsonDesconto;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'grupo_desconto');
          try
            JsonPairDesconto.Value.AsArray := JsonArrGrupoDesconto;
            AJson.Add('grupo_desconto').Assign(JsonPairDesconto);
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

procedure TBoletoW_Itau.GerarRecebimentoDivergente(AJson: TJsonObject);
var
  JsonRecDivergente: TJsonObject;
  JsonPairRecDivergente: TJsonPair;

begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonRecDivergente := TJSONObject.Create;

        try
          JsonRecDivergente.Add('tipo_autorizacao_recebimento').Value.AsString :=
            IntToStr(integer(TipoPagamento) + 1);

          if TipoPagamento in [tpAceita_Valores_entre_Minimo_Maximo, tpSomente_Valor_Minimo] then
          begin
            if (ValorMinPagamento > 0) and (ValorMaxPagamento > 0) then
              JsonRecDivergente.Add('tipo_valor_percentual_recebimento').Value.AsString := 'V'
            else
              JsonRecDivergente.Add('tipo_valor_percentual_recebimento').Value.AsString := 'P';
            JsonRecDivergente.Add('valor_minimo_recebimento').Value.AsString :=
              IntToStrZero(round(ValorMinPagamento * 100), 17);
            JsonRecDivergente.Add('percentual_minimo_recebimento').Value.AsString :=
              IntToStrZero(round(PercentualMinPagamento * 100000), 12);
            JsonRecDivergente.Add('valor_maximo_recebimento').Value.AsString :=
              IntToStrZero(round(ValorMaxPagamento * 100), 17);
            JsonRecDivergente.Add('percentual_maximo_recebimento').Value.AsString :=
              IntToStrZero(round(PercentualMaxPagamento * 100000), 12);
          end;

          JsonPairRecDivergente := TJsonPair.Create(AJson, 'recebimento_divergente');
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

constructor TBoletoW_Itau.Create(ABoletoWS: TBoletoWS);
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

function TBoletoW_Itau.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;

end;

function TBoletoW_Itau.Enviar: boolean;
begin
  Result := inherited Enviar;
end;


end.
