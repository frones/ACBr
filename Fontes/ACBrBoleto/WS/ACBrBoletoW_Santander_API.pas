{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  Jéter Rabelo Ferreira                          }

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

unit ACBrBoletoW_Santander_API;

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

  { TBoletoW_Santander_API }
  TBoletoW_Santander_API = class(TBoletoWSREST)
  private
    function GetNsu: string;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    procedure DefinirParamOAuth; override;
    procedure DefinirIdentificador;
    procedure RequisicaoJson;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure RequisicaoBaixa;
    procedure RequisicaoProtestar;
    procedure RequisicaoAltera;
    procedure RequisicaoAlterarDesconto(AJson: TJsonObject);
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarSacadorAvalista(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarProtesto(AJson: TJsonObject);
    procedure GerarRecebimentoDivergente(AJson: TJsonObject);
    procedure GerarPIX(AJson: TJsonObject);
    procedure GerarMensagens(AJson: TJsonObject);

    function GerarTokenAutenticacao: string; override;
    function ValidaAmbiente: string;
    function DefinirParametros:String;
    function retornaTipoDesconto(ATipo: TACBrTipoDesconto): string;
  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;
const
  C_URL = 'https://trust-open.api.santander.com.br/collection_bill_management/v2';
  C_URL_HOM = 'https://trust-sandbox.api.santander.com.br/collection_bill_management/v2';
  C_URL_OAUTH_HOM = 'https://trust-sandbox.api.santander.com.br/auth/oauth/v2/token';
  C_URL_OAUTH_PROD = 'https://trust-open.api.santander.com.br/auth/oauth/v2/token';

implementation

uses
  StrUtils,
  ACBrUtil.Strings,
  ACBrPixBase;

{ TBoletoW_Santander_API }

procedure TBoletoW_Santander_API.DefinirURL;
begin
  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM);
  case Boleto.Configuracoes.WebService.Operacao of
     tpInclui         : FPURL := FPURL + '/workspaces/' + Boleto.Cedente.CedenteWS.KeyUser + '/bank_slips';
     tpAltera         : FPURL := FPURL + '/workspaces/' + Boleto.Cedente.CedenteWS.KeyUser + '/bank_slips';
     tpConsulta       : FPURL := FPURL + '/bills?' +  DefinirParametros;
     tpConsultaDetalhe: FPURL := FPURL + '/bills/' +  DefinirParametros;
     tpBaixa          : FPURL := FPURL + '/workspaces/' + Boleto.Cedente.CedenteWS.KeyUser + '/bank_slips';
  end;
end;

procedure TBoletoW_Santander_API.DefinirContentType;
begin
  FPContentType := 'application/json';
end;

procedure TBoletoW_Santander_API.DefinirIdentificador;
begin
  if Assigned(ATitulo) then
    FPIdentificador := 'X-Application-Key: ' + ATitulo.ACBrBoleto.Cedente.CedenteWS.ClientID;
end;

procedure TBoletoW_Santander_API.GerarHeader;
begin
  DefinirContentType;
  DefinirIdentificador;
end;

procedure TBoletoW_Santander_API.GerarDados;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui: RequisicaoJson;
        tpAltera:
        begin
          FMetodoHTTP := htPATCH; // Define Método PATCH conforme manual do banco
          RequisicaoAltera;
        end;
        tpBaixa :
        begin
          FMetodoHTTP := htPATCH; // Define Método PATCH conforme manual do banco
          RequisicaoBaixa;
        end;
        tpConsulta:
        begin
          FMetodoHTTP := htGET; // Define Método GET Consulta
          RequisicaoConsulta;
        end;
        tpConsultaDetalhe:
        begin
          FMetodoHTTP := htGET; // Define Método GET Consulta
          RequisicaoConsultaDetalhe;
        end;
      else
          raise EACBrBoletoWSException.Create(ClassName + Format(
            S_OPERACAO_NAO_IMPLEMENTADO, [
            TipoOperacaoToStr(
            Boleto.Configuracoes.WebService.Operacao)]));
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_Santander_API.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
  BoletoWS.DoLog('Token Gerado: ' + Result);
end;

function TBoletoW_Santander_API.GetNsu: string;
begin
  Result := ATitulo.NossoNumero;
  if Boleto.Configuracoes.WebService.Ambiente = taHomologacao then
    Result := 'TST' + Result;
end;

function TBoletoW_Santander_API.DefinirParametros: String;
var
  Consulta: TStringList;
  ANossoNumero: String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin

    {
    1. default: Pesquisa padrão, trazendo somente dados básicos do boleto
    2. duplicate: Pesquisa de dados para emissão de segunda via de boleto
    3. bankslip: Pesquisa para dados completos do boleto
    4. settlement: Pesquisa para informações de baixas/liquidações do boleto
    5. registry: Pesquisa de informações de cartório do boleto
    }
    if Assigned(ATitulo) then
      ANossoNumero := ATitulo.NossoNumero;

    Consulta := TStringList.Create;
    try
      case Boleto.Configuracoes.WebService.Operacao of
        tpConsulta:
          case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
            isbBaixado:
              begin
                //settlement: Pesquisa para informações de baixas/liquidações do boleto
                Consulta.Add('beneficiaryCode='+ Boleto.Cedente.Convenio);
                Consulta.Add('bankNumber='+ ANossoNumero);
                Consulta.Add('tipoConsulta=settlement');
              end;
            isbAberto:
              begin
                // bankslip: Pesquisa para dados completos do boleto
                Consulta.Add('beneficiaryCode='+ Boleto.Cedente.Convenio);
                Consulta.Add('bankNumber='+ ANossoNumero);
                Consulta.Add('tipoConsulta=bankslip');
              end;
          end;
        tpConsultaDetalhe:
          case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
            isbBaixado:
              begin
                //settlement: Pesquisa para informações de baixas/liquidações do boleto
                Consulta.Add(Boleto.Cedente.Convenio + '.' + ANossoNumero);
                Consulta.Add('tipoConsulta=settlement');
              end;
            isbAberto:
              begin
                // bankslip: Pesquisa para dados completos do boleto
                Consulta.Add(Boleto.Cedente.Convenio + '.' + ANossoNumero);
                Consulta.Add('tipoConsulta=bankslip');
              end;
          end;
      end;
    finally
      case Boleto.Configuracoes.WebService.Operacao of
         tpConsulta:
           Consulta.Delimiter := '&';
         tpConsultaDetalhe:
           Consulta.Delimiter := '?';
      end;
      result := Consulta.DelimitedText;
      Consulta.Free;
    end;

  end;

end;

procedure TBoletoW_Santander_API.DefinirParamOAuth;
begin
  FParamsOAuth := C_GRANT_TYPE + '=client_credentials' +
    '&client_id=' + Boleto.Cedente.CedenteWS.ClientID +
    '&client_secret=' + Boleto.Cedente.CedenteWS.ClientSecret;
end;

function TBoletoW_Santander_API.ValidaAmbiente: string;
begin
  if Boleto.Configuracoes.WebService.Ambiente = taProducao then
    Result := 'PRODUCAO'
  else
    Result :=  'TESTE';
end;

procedure TBoletoW_Santander_API.RequisicaoAltera;
var
  Data: string;
  Json: TJSONObject;
begin
 if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      Json := TJsonObject.Create;
      try
        Json.Add('covenantCode').Value.AsString := Boleto.Cedente.Convenio;
        Json.Add('bankNumber').Value.AsString   := NossoNumero;

        case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
          3:  // RemessaConcederAbatimento
            begin
              Json.Add('deductionValue').Value.AsString   := StringReplace(FormatFloat('0.00', ValorAbatimento), ',', '.', [rfReplaceAll]);
            end;
          4:  // RemessaCancelarAbatimento
            begin
              Json.Add('deductionValue').Value.AsString   := StringReplace(FormatFloat('0.00', 0), ',', '.', [rfReplaceAll]);
            end;
          5: //RemessaConcederDesconto
            begin
              RequisicaoAlterarDesconto(Json)
            end;
          7: //RemessaAlterarVencimento
            begin
               Json.Add('dueDate').Value.AsString := FormatDateTime('yyyy-mm-dd', Vencimento);
            end;
          9:  //RemessaProtestar
            begin
               Json.Add('operation').Value.AsString    := 'PROTESTAR';
            end;
          10:  //RemessaSustarProtesto
            begin
               Json.Add('operation').Value.AsString    := 'BAIXAR';
            end;
          12:  //RemessaCancelarInstrucaoProtesto
            begin
               Json.Add('operation').Value.AsString    := 'CANCELAR_PROTESTO';
            end;
          13:  //RemessaDispensarJuros
            begin
              Json.Add('interestPercentage').Value.AsString := StringReplace(FormatFloat('0.00', 0), ',', '.', [rfReplaceAll]);
            end;
          18:  //RemessaAlterarSeuNumero
            begin
              Json.Add('clientNumber').Value.AsString := NumeroDocumento; // seu número ou numero documento
            end;
          40:  //RemessaAlterarNumeroDiasProtesto,
            begin
               Json.Add('protestQuatilyDays').Value.AsString := IntToStr(DiasDeProtesto);
            end;
          50:  //RemessaAlterarMulta
            begin
               if PercentualMulta > 0 then
                begin
                  if MultaValorFixo then
                    raise Exception.Create('Permitido multa apenas como percentual!');
                  Json.Add('finePercentage').Value.AsString := StringReplace(FormatFloat('0.00', PercentualMulta), ',', '.', [rfReplaceAll]);
                  Json.Add('FineDate').Value.AsString := FormatDateTime('yyyy-mm-dd', DataMulta);
                end
            end;
          51:  //RemessaDispensarMulta
            begin
               Json.Add('finePercentage').Value.AsString := StringReplace(FormatFloat('0.00', 0), ',', '.', [rfReplaceAll]);
            end;
          52: //RemessaAlterarDesconto
            begin
              RequisicaoAlterarDesconto(Json) ;
            end;
          53: //toRemessaNaoConcederDesconto,
            begin
              RequisicaoAlterarDesconto(Json) ;
            end;
        end;
        Data := Json.Stringify;
        FPDadosMsg := Data;
      finally
        Json.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.RequisicaoBaixa;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      Json := TJsonObject.Create;
      try
        Json.Add('covenantCode').Value.AsString := Boleto.Cedente.Convenio;
        Json.Add('bankNumber').Value.AsString   := NossoNumero;
        Json.Add('operation').Value.AsString    := 'BAIXAR';

        Data := Json.Stringify;
        FPDadosMsg := Data;
      finally
        Json.Free;
      end;
    end;
  end;
end;


procedure TBoletoW_Santander_API.RequisicaoConsulta;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Santander_API.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET

end;

procedure TBoletoW_Santander_API.RequisicaoJson;
var
  Data, AEspecieDoc: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if AnsiSameText(EspecieDoc,'DM') then
         AEspecieDoc:= 'DUPLICATA_MERCANTIL'
      else if AnsiSameText(EspecieDoc, 'NP') then
         AEspecieDoc:= 'NOTA_PROMISSORIA'
      else if AnsiSameText(EspecieDoc, 'NS') then
         AEspecieDoc:= 'DUPLICATA_SERVICO'
      else if AnsiSameText(EspecieDoc, 'REC') then
         AEspecieDoc:= 'RECIBO'
      else
         AEspecieDoc:= 'OUTROS';

      Json := TJsonObject.Create;
      try
        Json.Add('nsuCode').Value.AsString := GetNsu;
        Json.Add('environment').Value.AsString := ValidaAmbiente;
        Json.Add('nsuDate').Value.AsString := FormatDateTime('yyyy-mm-dd', Now);
        Json.Add('covenantCode').Value.AsString := Boleto.Cedente.Convenio;
        Json.Add('bankNumber').Value.AsString := NossoNumero;
        Json.Add('clientNumber').Value.AsString := NumeroDocumento;
        Json.Add('dueDate').Value.AsString := FormatDateTime('yyyy-mm-dd', Vencimento);
        Json.Add('issueDate').Value.AsString := FormatDateTime('yyyy-mm-dd', Now);
//        Json.Add('participantCode').Value.AsString := ''; // Identificação de Controle do Participante (participantCode):
        Json.Add('nominalValue').Value.AsString := StringReplace(FormatFloat('0.00', ValorDocumento), ',', '.', [rfReplaceAll]);

        GerarPagador(Json);
        GerarSacadorAvalista(Json);

        Json.Add('documentKind').Value.AsString := AEspecieDoc;
        GerarDesconto(Json);
        GerarMulta(Json);
        GerarJuros(Json);
        if ValorAbatimento > 0 then
          Json.Add('deductionValue').Value.AsString := StringReplace(FormatFloat('0.00', ValorAbatimento), ',', '.', [rfReplaceAll]);
        GerarProtesto(Json);
        GerarRecebimentoDivergente(Json);
        GerarPix(Json);
        if QrCode.txId <> '' then
          Json.Add('txId').Value.AsString := QrCode.txId;
        GerarMensagens(Json);

        Data := Json.Stringify;

        FPDadosMsg := Data;
        //WriteToTXT('C:\temp\json.txt', Data, False, False);
      finally
        Json.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.RequisicaoProtestar;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      Json := TJsonObject.Create;
      try
        Json.Add('covenantCode').Value.AsString := Boleto.Cedente.Convenio;
        Json.Add('bankNumber').Value.AsString   := NossoNumero;
        Json.Add('operation').Value.AsString    := 'PROTESTAR';
        GerarMensagens(Json);
        Data := Json.Stringify;
        FPDadosMsg := Data;
      finally
        Json.Free;
      end;
    end;
  end;
end;

function TBoletoW_Santander_API.retornaTipoDesconto(
  ATipo: TACBrTipoDesconto): string;
begin
  result := '';
    case ATipo of
      tdNaoConcederDesconto:
        Result := 'ISENTO';
      tdValorFixoAteDataInformada:
        Result := 'VALOR_DATA_FIXA';
      tdValorAntecipacaoDiaCorrido:
        Result := 'VALOR_DIA_CORRIDO';
      tdValorAntecipacaoDiaUtil:
        Result := 'VALOR_DIA_UTIL ';
      else
        raise Exception.Create('Modalidade de desconto não permitida');
    end;
end;

procedure TBoletoW_Santander_API.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJsonPair;
  SCnpjCpf: string;
  SCep: string;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDadosPagador := TJSONObject.Create;
        SCnpjCpf := OnlyNumber(Sacado.CNPJCPF);
        try
          JsonDadosPagador.Add('name').Value.AsString := Copy(Sacado.NomeSacado, 1, 40);
          if Length(SCnpjCpf) <= 11 then
            JsonDadosPagador.Add('documentType').Value.AsString := 'CPF'
          else
            JsonDadosPagador.Add('documentType').Value.AsString := 'CNPJ';
          JsonDadosPagador.Add('documentNumber').Value.AsString := SCnpjCpf;
          JsonDadosPagador.Add('address').Value.AsString        := Trim(Copy(Sacado.Logradouro + ' ' + Sacado.Numero, 1, 40));
          JsonDadosPagador.Add('neighborhood').Value.AsString   := Copy(Sacado.Bairro, 1, 30);
          JsonDadosPagador.Add('city').Value.AsString           := Copy(Sacado.Cidade, 1, 20);
          JsonDadosPagador.Add('state').Value.AsString          := Sacado.UF;
          SCep := ACBrUtil.Strings.PadLeft(OnlyNumber(Sacado.CEP), 8, '0');
          JsonDadosPagador.Add('zipCode').Value.AsString        := Copy(SCep, 1, 5) + '-' + Copy(SCep, 6, 3);
          JsonPairPagador := TJsonPair.Create(AJson, 'payer');
          try
            JsonPairPagador.Value.AsObject := JsonDadosPagador;
            AJson.Add('payer').Assign(JsonPairPagador);
          finally
            JsonPairPagador.Free;
          end;
        finally
          JsonDadosPagador.Free;
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.GerarPIX(AJson: TJsonObject);
var
  JsonLocal: TJsonObject;
  JsonPairLocal: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if ACBrBoleto.Cedente.PIX.TipoChavePIX = tchNenhuma then
        Exit;
      if Assigned(AJson) then
      begin
        JsonLocal := TJSONObject.Create;
        try
          case ACBrBoleto.Cedente.PIX.TipoChavePIX of
            tchEmail: JsonLocal.Add('type').Value.AsString := 'EMAIL';
            tchCPF: JsonLocal.Add('type').Value.AsString := 'CPF';
            tchCNPJ: JsonLocal.Add('type').Value.AsString := 'CNPJ';
            tchCelular: JsonLocal.Add('type').Value.AsString := 'CELULAR';
            tchAleatoria: JsonLocal.Add('type').Value.AsString := 'EVP';
          end;
          JsonLocal.Add('dictKey').Value.AsString := ACBrBoleto.Cedente.PIX.Chave;
          JsonPairLocal := TJsonPair.Create(AJson, 'key');
          try
            JsonPairLocal.Value.AsObject := JsonLocal;
            AJson.Add('key').Assign(JsonPairLocal);
          finally
            JsonPairLocal.Free;
          end;
        finally
          JsonLocal.Free;
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.GerarProtesto(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        if DiasDeProtesto = 0 then
        begin
          AJson.Add('protestType').Value.AsString := 'SEM_PROTESTO';
        end
        else
        begin
          case TipoDiasProtesto of
            diCorridos:
              AJson.Add('protestType').Value.AsString := 'DIAS_CORRIDOS';
            diUteis:
              AJson.Add('protestType').Value.AsString := 'DIAS_UTEIS';
          end;
          AJson.Add('protestQuantityDays').Value.AsString := IntToStr(DiasDeProtesto);
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.GerarSacadorAvalista(AJson: TJsonObject);
var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJsonPair;
  SCnpjCpf: string;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      SCnpjCpf := OnlyNumber(Sacado.SacadoAvalista.CNPJCPF);
      if SCnpjCpf = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        JsonSacadorAvalista := TJSONObject.Create;
        try
          JsonSacadorAvalista.Add('name').Value.AsString := Sacado.SacadoAvalista.NomeAvalista;
          if Length(SCnpjCpf) <= 11 then
            JsonSacadorAvalista.Add('documentType').Value.AsString := 'CPF'
          else
            JsonSacadorAvalista.Add('documentType').Value.AsString := 'CNPJ';
          JsonSacadorAvalista.Add('documentNumber').Value.AsString := SCnpjCpf;
          JsonPairSacadorAvalista := TJsonPair.Create(AJson, 'beneficiary');
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
end;

procedure TBoletoW_Santander_API.GerarMensagens(AJson: TJsonObject);
var
  JsonLocal: TJsonObject;
  JsonArrLocal: TJsonArray;
  JsonPairLocal: TJsonPair;
  I: Integer;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonLocal := TJSONObject.Create;
        JsonArrLocal := TJsonArray.Create;
        try
          for I := 0 to Mensagem.Count-1 do
          begin
            JsonArrLocal.Add.AsString := Mensagem[I];
          end;

          JsonPairLocal := TJsonPair.Create(AJson, 'messages');
          try
            JsonPairLocal.Value.AsArray := JsonArrLocal;
            AJson.Add('messages').Assign(JsonPairLocal);
          finally
            JsonPairLocal.Free;
          end;
        finally
          JsonArrLocal.Free;
          JsonLocal.Free;
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.GerarMulta(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        if PercentualMulta > 0 then
        begin
          if MultaValorFixo then
            raise Exception.Create('Permitido multa apenas como percentual!');
          AJson.Add('finePercentage').Value.AsString := StringReplace(FormatFloat('0.00', PercentualMulta), ',', '.', [rfReplaceAll]);
          if DataMulta > 0 then
            AJson.Add('fineQuantityDays').Value.AsString := IntToStr(Abs(Trunc(Vencimento - DataMulta)))
          else
            AJson.Add('fineQuantityDays').Value.AsString := '0';
        end
        else
        begin
          AJson.Add('finePercentage').Value.AsString := '0';
          AJson.Add('fineQuantityDays').Value.AsString := '0';
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.GerarJuros(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        if not (CodigoMoraJuros in [cjTaxaMensal, cjIsento]) then
          raise Exception.Create('Permitido apenas taxa mensal de juros');
        AJson.Add('interestPercentage').Value.AsString := StringReplace(FormatFloat('0.00', ValorMoraJuros), ',', '.', [rfReplaceAll]);
      end;
    end;
  end;
end;

procedure TBoletoW_Santander_API.RequisicaoAlterarDesconto(AJson: TJsonObject);
var
  JsonDesconto : TJsonObject;
  JsonPairDesconto: TJsonPair;
  SValorDesconto1, SValorDesconto2, SValorDesconto3, STipo1, STipo2, STipo3 : string;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDesconto := TJsonObject.Create;
        try
          if DataDesconto > 0 then
          begin
            case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
               6,53: // toRemessaCancelarDesconto,  toRemessaNaoConcederDesconto
               begin
                  SValorDesconto1:='0.00';
                  SValorDesconto2:='0.00';
                  SValorDesconto3:='0.00';
                  STipo1 :=  'ISENTO';
                  STipo2 :=  'ISENTO';
                  STipo3 :=  'ISENTO';
               end;
               5,52:  //    toRemessaConcederDesconto,  toRemessaAlterarDesconto,
               begin
                 SValorDesconto1 := IfThen(TipoDesconto=tdNaoConcederDesconto,'0.00',
                                           StringReplace(FormatFloat('0.00', ValorDesconto), ',', '.', [rfReplaceAll]));
                 SValorDesconto2 := IfThen(TipoDesconto2=tdNaoConcederDesconto,'0.00',
                                           StringReplace(FormatFloat('0.00', ValorDesconto2), ',', '.', [rfReplaceAll]));
                 SValorDesconto3 := IfThen(TipoDesconto3=tdNaoConcederDesconto,'0.00',
                                           StringReplace(FormatFloat('0.00', ValorDesconto3), ',', '.', [rfReplaceAll]));
                 STipo1 := retornaTipoDesconto(TipoDesconto);
                 STipo2 := retornaTipoDesconto(TipoDesconto2);
                 STipo3 := retornaTipoDesconto(TipoDesconto3);
               end;
            end;
            JsonDesconto.Add('type').Value.AsString      := STipo1;
            JsonDesconto.Add('value').Value.AsString     := SValorDesconto1;
            JsonDesconto.Add('limitDate').Value.AsString := FormatDateTime('yyyy-mm-dd', DataDesconto);
            JsonPairDesconto := TJSONPair.Create(AJson, 'discountOne');
            try
              JsonPairDesconto.Value.AsObject := JsonDesconto;
              AJson.Add('discountOne').Assign(JsonPairDesconto);
            finally
              JsonPairDesconto.Free;
            end;
          end;
          if DataDesconto2 > 0 then
          begin
            JsonDesconto := TJsonObject.Create;
            JsonDesconto.Add('type').Value.AsString      := STipo1;
            JsonDesconto.Add('value').Value.AsString     := SValorDesconto2;
            JsonDesconto.Add('limitDate').Value.AsString := FormatDateTime('yyyy-mm-dd', DataDesconto2);
            JsonPairDesconto := TJSONPair.Create(AJson, 'discountTwo');
            try
              JsonPairDesconto.Value.AsObject := JsonDesconto;
              AJson.Add('discountTwo').Assign(JsonPairDesconto);
            finally
              JsonPairDesconto.Free;
            end;
          end;

          if DataDesconto3 > 0 then
          begin
            JsonDesconto := TJsonObject.Create;
            JsonDesconto.Add('type').Value.AsString      := STipo3;
            JsonDesconto.Add('value').Value.AsString     := SValorDesconto3;
            JsonDesconto.Add('limitDate').Value.AsString := FormatDateTime('yyyy-mm-dd', DataDesconto3);
            JsonPairDesconto := TJSONPair.Create(AJson, 'discountThree');
            try
              JsonPairDesconto.Value.AsObject := JsonDesconto;
              AJson.Add('discountThree').Assign(JsonPairDesconto);
            finally
              JsonPairDesconto.Free;
            end;
          end;
        finally
          JsonDesconto.Free;
        end;
      end;
    end;
  end;

end;


procedure TBoletoW_Santander_API.GerarDesconto(AJson: TJsonObject);
var
  JsonDesconto: TJsonObject;
  JsonPairGrupoDesconto: TJsonPair;
  JsonPairDesconto: TJsonPair;
  STipoDesconto: string;
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        JsonDesconto := TJSONObject.Create;
        try
          case TipoDesconto of
            tdNaoConcederDesconto:
              STipoDesconto := 'ISENTO';
            tdValorFixoAteDataInformada:
              STipoDesconto := 'VALOR_DATA_FIXA';
            tdValorAntecipacaoDiaCorrido:
              STipoDesconto := 'VALOR_DIA_CORRIDO';
            tdValorAntecipacaoDiaUtil:
              STipoDesconto := 'VALOR_DIA_UTIL ';
            else
              raise Exception.Create('Modalidade de desconto não permitida');
          end;
          JsonDesconto.Add('type').Value.AsString := STipoDesconto;
          if DataDesconto > 0 then
          begin
            JsonDesconto.Add('value').Value.AsString :=
              StringReplace(FormatFloat('0.00', ValorDesconto), ',', '.', [rfReplaceAll]);
            JsonDesconto.Add('limitDate').Value.AsString :=
              FormatDateTime('yyyy-mm-dd', DataDesconto);
            JsonPairGrupoDesconto := TJsonPair.Create(AJson, 'discountOne');
            try
              JsonPairGrupoDesconto.Value.AsObject := JsonDesconto;
              JsonDesconto.Add('discountOne').Assign(JsonPairGrupoDesconto);
            finally
              JsonPairGrupoDesconto.Free;
            end;
          end;

          //Desconto2
          if ValorDesconto2 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('value').Value.AsString :=
              StringReplace(FormatFloat('0.00', ValorDesconto2), ',', '.', [rfReplaceAll]);
            JsonDesconto.Add('limitDate').Value.AsString :=
              FormatDateTime('yyyy-mm-dd', DataDesconto2);
            JsonPairGrupoDesconto := TJsonPair.Create(AJson, 'discountTwo');
            try
              JsonPairGrupoDesconto.Value.AsObject := JsonDesconto;
              JsonDesconto.Add('discountTwo').Assign(JsonPairGrupoDesconto);
            finally
              JsonPairGrupoDesconto.Free;
            end;
          end;

          //Desconto3
          if ValorDesconto3 > 0 then
          begin
            JsonDesconto.Clear;
            JsonDesconto.Add('value').Value.AsString :=
              StringReplace(FormatFloat('0.00', ValorDesconto3), ',', '.', [rfReplaceAll]);
            JsonDesconto.Add('limitDate').Value.AsString :=
              FormatDateTime('yyyy-mm-dd', DataDesconto3);
            JsonPairGrupoDesconto := TJsonPair.Create(AJson, 'discountThree');
            try
              JsonPairGrupoDesconto.Value.AsObject := JsonDesconto;
              JsonDesconto.Add('discountThree').Assign(JsonPairGrupoDesconto);
            finally
              JsonPairGrupoDesconto.Free;
            end;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'discount');
          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('discount').Assign(JsonPairDesconto);
          finally
            JsonPairDesconto.Free;
          end;
        finally
          JsonDesconto.Free;
        end;
      end;
    end;
  end;

end;

procedure TBoletoW_Santander_API.GerarRecebimentoDivergente(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    with ATitulo do
    begin
      if Assigned(AJson) then
      begin
        {Nota 15: Tipo de Pagamento (paymentType):
                  Informação obrigatória que indica o tipo de pagamento a ser atribuído para o boleto:
                  Informar:
                  • REGISTRO: Permitirá o pagamento do boleto somente pelo valor nominal calculado (juros, multa, desconto e abatimento);
                  • DIVERGENTE: Permitirá o pagamento do boleto por um range de valor, sendo definido no registro um valor/percentual mínimo e máximo;
                  • PARCIAL: Permitirá até 99 pagamentos para o mesmo boleto, por um range de valor, sendo definido no registro um valor/percentual mínimo e máximo;

        Nota 16: Quantidade de Parciais (parcelsQuantity):
                 Campo numérico obrigatório para os boletos onde houve a indicação do tipo de pagamento “PARCIAL”, indica a quantidade de pagamentos que será permitida para o mesmo boleto, sendo o máximo de 99.

        Nota 17: Tipo de Valor (valueType):
                 Campo obrigatório para os boletos onde houve a indicação do tipo de pagamento “DIVERGENTE” ou “PARCIAL”. Esse campo definirá se os valores mínimo e máximo para pagamento do boleto serão expressos em valor ou percentual.
                 Informar:
                 • PERCENTUAL
                 • VALOR

        Nota 18: Valor mínimo ou percentual mínimo do boleto (minValueOrPercentage):
                 Identifica o valor mínimo ou percentual mínimo do boleto:
                 • Quando o tipo de valor informado for PERCENTUAL considerar a seguinte formatação 9(10)V99999
                 • Quando o tipo de valor informado for VALOR considerar a seguinte formatação 9(10)V99000

        Nota 19: Valor máximo ou percentual máximo do boleto (maxValueOrPercentage):
                 Identifica o valor máximo ou percentual máximo do boleto:
                 • Quando o tipo de valor informado for PERCENTUAL considerar a seguinte formatação 9(10)V99999
                 • Quando o tipo de valor informado for VALOR considerar a seguinte formatação 9(10)V99000

        obs.: Implementado apenas a opção de pagamento do boleto, sem valor parcial ou parcelas
}
        AJson.Add('paymentType').Value.AsString := 'REGISTRO';
      end;
    end;
  end;
end;

constructor TBoletoW_Santander_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := '';

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := True;
  end;

end;

function TBoletoW_Santander_API.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;

end;

function TBoletoW_Santander_API.Enviar: boolean;
begin
  Result := inherited Enviar;
end;


end.
