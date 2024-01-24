  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }

  { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

  { Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Leandro do Couto,}
  { Delmar de Lima                                                               }
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
unit ACBrBoletoW_Safra;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  StrUtils,
  ACBrBoleto,
  Jsons,
  ACBrBoletoWS.Rest;

type

    { TBoletoW_Safra }
  TBoletoW_Safra = class(TBoletoWSREST)
  private
    function DateTimeToDateInter(const AValue: TDateTime): String;
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
    procedure GerarDocumento(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GerarMultaJuros(AJson: TJsonObject);
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarEnderecoPagador(AJson: TJsonObject);
    procedure GeraMensagem(AJson: TJsonObject);

    procedure GerarBenificiarioFinal(AJson: TJsonObject);

    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;

    procedure GeraDadosInstrucao(AJson: TJsonObject);
  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL     = ' https://api.safranegocios.com.br/gateway/cobrancas/v1';
  C_URL_HOM = 'https://api-hml.safranegocios.com.br/gateway/cobrancas/v1';

  C_URL_OAUTH_PROD = 'https://api.safranegocios.com.br/gateway/v1/oauth2/token';
  C_URL_OAUTH_HOM  = 'https://api-hml.safranegocios.com.br/gateway/v1/oauth2/token';

  C_CONTENT_TYPE  = 'application/json';
  C_ACCEPT        = 'application/json';
  C_AUTHORIZATION = 'Authorization';
  C_IDENTIFICADOR = 'Safra-Correlation-ID: 41fa65a3-1a71-4437-a169-5bd209eb2d3a';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET        = 'utf-8';
  C_ACCEPT_CHARSET = 'utf-8';

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Math;

  { TBoletoW_Safra }

procedure TBoletoW_Safra.DefinirURL;
var
  aNossoNumero: string;
begin
  if (aTitulo <> nil) then
    aNossoNumero := PadLeft(aTitulo.NossoNumero, 11, '0');

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      FPURL := FPURL + '/boletos';
    tpConsulta:
      FPURL := FPURL + '/boletos?' + DefinirParametros;
  end;
end;

procedure TBoletoW_Safra.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Safra.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_Safra.GerarDados;
begin
  if Assigned(Boleto) then
    DefinirURL;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        FMetodoHTTP := htPOST; // Define Método POST para Incluir
        RequisicaoJson;
      end;

    tpConsulta:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta
        RequisicaoConsulta;
      end;
    else
      raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO, [ TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao) ]));
  end;
end;

procedure TBoletoW_Safra.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_Safra.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result        := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Safra.DefinirKeyUser;
begin
  if Assigned(aTitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Safra.DefinirParametros: String;
var
  Consulta                : TStringList;
  LNossoNumero, LSeuNumero: String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    Consulta           := TStringList.Create;
    Consulta.Delimiter := '&';
    if Assigned(aTitulo) then
    begin
      LNossoNumero := aTitulo.NossoNumero;
      LSeuNumero   := aTitulo.SeuNumero;
    end;
    try
      Consulta.Add('agencia=' + IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Agencia, ''));
      Consulta.Add('conta=' + IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Conta, ''));

      if LNossoNumero <> '' then
        Consulta.Add('numero=' + LNossoNumero);

      if LSeuNumero <> '' then
        Consulta.Add('numeroCliente=' + LSeuNumero);
    finally
      Result := Consulta.DelimitedText;
      Consulta.Free;
    end;
  end;
end;

procedure TBoletoW_Safra.DefinirParamOAuth;
begin
  FParamsOAuth := Format('client_id=%s&username=%s&password=%s&refresh_token=&grant_type=password', [ Boleto.Cedente.CedenteWS.ClientID, Boleto.Cedente.CedenteWS.KeyUser,
      Boleto.Cedente.CedenteWS.ClientSecret ]);
end;

function TBoletoW_Safra.DateTimeToDateInter(const AValue: TDateTime): String;
begin
  Result := FormatDateBr(AValue, 'YYYY-MM-DD');
end;

procedure TBoletoW_Safra.DefinirAutenticacao;
begin

end;

function TBoletoW_Safra.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1', '2'), 2);
end;

procedure TBoletoW_Safra.RequisicaoJson;
var
  Data: string;
  Json: TJsonObject;
begin
  if Assigned(aTitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('agencia').Value.asString := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Agencia, '12400');
      Json.Add('conta').Value.asString := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Conta + aTitulo.ACBrBoleto.Cedente.ContaDigito,
        '008554440');

      GerarDocumento(Json);

      Data       := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarDocumento(AJson: TJsonObject);
const
  EPC_DUPLICATA_MERCANTIL  = '01';
  EPC_NOTA_PROMISSORIA     = '02';
  EPC_NOTA_DE_SEGURO       = '03';
  EPC_RECIBO               = '05';
  EPC_DUPLICATA_DE_SERVICO = '09';
var
  JsonDocumento    : TJsonObject;
  JsonPairDocumento: TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDocumento := TJsonObject.Create;
      try
        JsonDocumento.Add('numero').Value.asString         := Copy(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 1, 15);
        JsonDocumento.Add('numeroCliente').Value.asString  := Copy(aTitulo.SeuNumero, 1, 10);
        JsonDocumento.Add('diasDevolucao').Value.AsInteger := 90;
        JsonDocumento.Add('especie').Value.asString        := EPC_DUPLICATA_MERCANTIL;
        JsonDocumento.Add('dataVencimento').Value.asString := DateTimeToDateInter(aTitulo.Vencimento);
        JsonDocumento.Add('valor').Value.asNumber          := aTitulo.ValorDocumento;
        JsonDocumento.Add('codigoMoeda').Value.AsInteger   := 0;
        JsonDocumento.Add('quantidadeDiasProtesto').Value.AsInteger := aTitulo.DiasDeProtesto;
        JsonDocumento.Add('campoLivre').Value.asString := Copy('', 1, 25);
        JsonDocumento.Add('fidc').Value.AsInteger      := 0;
        JsonDocumento.Add('danfe').Value.asString      := Copy('', 1, 44);
        JsonDocumento.Add('valorAbatimento').Value.asNumber := aTitulo.ValorAbatimento;
          //            JsonDocumento.Add('identificacaoAceite').Value.AsString     := aTitulo.Aceite.atSim;

        GerarDesconto(JsonDocumento);
        GerarMultaJuros(JsonDocumento);
        GerarPagador(JsonDocumento);
        GeraMensagem(JsonDocumento);

        JsonPairDocumento := TJSONPair.Create(AJson, 'Documento');
        try
          JsonPairDocumento.Value.AsObject := JsonDocumento;
          AJson.Add('Documento').Assign(JsonPairDocumento);
        finally
          JsonPairDocumento.Free;
        end;

      finally
        JsonDocumento.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarDesconto(AJson: TJsonObject);
Const
  C_DESCONTO_EM_VALOR                                    = 1;
  C_DESCONTO_EM_PERCENTUAL                               = 2;
  C_DESCONTO_EM_VALOR_POR_ANTECIPACAO_DIAS_CORRIDOS      = 3;
  C_DESCONTO_EM_PERCENTUAL_POR_ANTECIPACAO_DIAS_CORRIDOS = 5;
var
  JsonDesconto    : TJsonObject;
  JsonPairDesconto: TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (aTitulo.ValorDesconto + aTitulo.ValorDesconto2 + aTitulo.ValorDesconto3) > 0 then
      begin
        JsonDesconto := TJsonObject.Create;
        try
          JsonDesconto := TJsonObject.Create;

          if (aTitulo.DataDesconto > 0) then
          begin
            JsonDesconto.Add('data').Value.asString  := DateTimeToDateInter(aTitulo.DataDesconto);
            JsonDesconto.Add('valor').Value.asNumber := RoundABNT(aTitulo.ValorDesconto, 2);
          end;

          if (aTitulo.DataDesconto2 > 0) then
          begin
            JsonDesconto.Add('data2').Value.asString  := DateTimeToDateInter(aTitulo.DataDesconto2);
            JsonDesconto.Add('valor2').Value.asNumber := RoundABNT(aTitulo.ValorDesconto2, 2);
          end;

          if (aTitulo.DataDesconto3 > 0) then
          begin
            JsonDesconto.Add('data3').Value.asString  := DateTimeToDateInter(aTitulo.DataDesconto3);
            JsonDesconto.Add('valor3').Value.asNumber := RoundABNT(aTitulo.ValorDesconto3, 2);
          end;

          JsonDesconto.Add('tipoDesconto ').Value.AsInteger := C_DESCONTO_EM_VALOR;

          JsonPairDesconto := TJSONPair.Create(AJson, 'desconto');
          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('desconto').Assign(JsonPairDesconto);
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

procedure TBoletoW_Safra.GerarMultaJuros(AJson: TJsonObject);
var
  JsonMultaJuros    : TJsonObject;
  JsonPairMultaJuros: TJSONPair;
  LMulta            : Double;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (aTitulo.PercentualMulta + aTitulo.ValorMoraJuros) > 0 then
      begin
        JsonMultaJuros := TJsonObject.Create;
        try
          JsonMultaJuros.Add('dataMulta').Value.asString := DateTimeToDateInter(aTitulo.DataMulta);
          if aTitulo.MultaValorFixo then
            LMulta := RoundABNT((aTitulo.PercentualMulta * 100) / aTitulo.ValorDocumento, 2)
          else
            LMulta                                       := aTitulo.PercentualMulta;
          JsonMultaJuros.Add('taxaMulta').Value.asNumber := aTitulo.PercentualMulta;

          JsonMultaJuros.Add('dataJuros').Value.asString := DateTimeToDateInter(aTitulo.DataMulta);

          JsonMultaJuros.Add('taxaJuros').Value.asNumber := aTitulo.ValorMoraJuros;

          JsonPairMultaJuros := TJSONPair.Create(AJson, 'multa');
          try
            JsonPairMultaJuros.Value.AsObject := JsonMultaJuros;
            AJson.Add('multa').Assign(JsonPairMultaJuros);
          finally
            JsonPairMultaJuros.Free;
          end;
        finally
          JsonMultaJuros.Free;
        end;
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador : TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDadosPagador := TJsonObject.Create;
      try
        JsonDadosPagador.Add('nome').Value.asString       := Copy(aTitulo.Sacado.NomeSacado, 1, 40);
        JsonDadosPagador.Add('tipoPessoa').Value.asString := IfThen(Length(OnlyNumber(aTitulo.Sacado.CNPJCPF)) = 11, 'F', 'J');
        JsonDadosPagador.Add('numeroDocumento').Value.asString := OnlyNumber(aTitulo.Sacado.CNPJCPF);
        JsonDadosPagador.Add('email').Value.asString := Copy(aTitulo.Sacado.Email, 1, 14);

        GerarEnderecoPagador(JsonDadosPagador);

        JsonPairPagador := TJSONPair.Create(AJson, 'pagador');
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

procedure TBoletoW_Safra.GerarEnderecoPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador : TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDadosPagador := TJsonObject.Create;
      try
        JsonDadosPagador.Add('logradouro').Value.asString := Copy(aTitulo.Sacado.Logradouro + ', ' + aTitulo.Sacado.Numero + ', ' + aTitulo.Sacado.Complemento, 1, 40);
        JsonDadosPagador.Add('bairro').Value.asString     := Copy(aTitulo.Sacado.Bairro, 1, 10);
        JsonDadosPagador.Add('cidade').Value.asString     := Copy(aTitulo.Sacado.Cidade, 1, 15);
        JsonDadosPagador.Add('uf').Value.asString         := aTitulo.Sacado.UF;
        JsonDadosPagador.Add('cep').Value.asString        := OnlyNumber(aTitulo.Sacado.CEP);

        JsonPairPagador := TJSONPair.Create(AJson, 'endereco');
        try
          JsonPairPagador.Value.AsObject := JsonDadosPagador;
          AJson.Add('endereco').Assign(JsonPairPagador);
        finally
          JsonPairPagador.Free;
        end;
      finally
        JsonDadosPagador.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GeraMensagem(AJson: TJsonObject);
Const
  TP_MENSAGEM_RECIBO = 1;
  TP_MENSAGEM_FICHA  = 2;
var
  JsonMensagem     : TJsonObject;
  JsonPairMensagem : TJSONPair;
  JsonArrayMensagem: TJsonArray;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonArrayMensagem := TJsonArray.Create;
      JsonMensagem      := TJsonObject.Create;
      try
        JsonMensagem.Add('posicao').Value.AsInteger := TP_MENSAGEM_RECIBO;
        JsonMensagem.Add('mensagem').Value.asString := Copy('', 1, 72);

        JsonPairMensagem := TJSONPair.Create(AJson, 'mensagem');
        try
          JsonPairMensagem.Value.AsArray := JsonArrayMensagem;
          AJson.Add('mensagem').Assign(JsonPairMensagem);
        finally
          JsonPairMensagem.Free;
        end;
      finally
        JsonArrayMensagem.Free;
        JsonMensagem.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GeraDadosInstrucao(AJson: TJsonObject);
begin
    // Sem Payload
end;

procedure TBoletoW_Safra.RequisicaoConsulta;
begin
    // Sem Payload - Define Método GET
end;

procedure TBoletoW_Safra.RequisicaoConsultaDetalhe;
begin
    // Sem Payload - Define Método GET
end;

procedure TBoletoW_Safra.GerarBenificiarioFinal(AJson: TJsonObject);
var
  JsonSacadorAvalista    : TJsonObject;
  JsonPairSacadorAvalista: TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if aTitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    if Assigned(AJson) then
    begin
      JsonSacadorAvalista := TJsonObject.Create;

      try
        JsonSacadorAvalista.Add('nome').Value.asString    := aTitulo.Sacado.SacadoAvalista.NomeAvalista;
        JsonSacadorAvalista.Add('cpfCnpj').Value.asString := OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF);
        JsonSacadorAvalista.Add('tipoPessoa').Value.asString := IfThen(Length(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, 'FISICA', 'JURIDICA');
        JsonSacadorAvalista.Add('cep').Value.asString      := aTitulo.Sacado.SacadoAvalista.CEP;
        JsonSacadorAvalista.Add('endereco').Value.asString := aTitulo.Sacado.SacadoAvalista.Logradouro;
        JsonSacadorAvalista.Add('bairro').Value.asString   := aTitulo.Sacado.SacadoAvalista.Bairro;
        JsonSacadorAvalista.Add('cidade').Value.asString   := aTitulo.Sacado.SacadoAvalista.Cidade;
        JsonSacadorAvalista.Add('uf').Value.asString       := aTitulo.Sacado.SacadoAvalista.UF;

        JsonPairSacadorAvalista := TJSONPair.Create(AJson, 'beneficiarioFinal');
        try
          JsonPairSacadorAvalista.Value.AsObject := JsonSacadorAvalista;
          AJson.Add('beneficiarioFinal').Assign(JsonPairSacadorAvalista);
        finally
          JsonPairSacadorAvalista.Free;
        end;
      finally
        JsonSacadorAvalista.Free;
      end;
    end;

  end;

end;

constructor TBoletoW_Safra.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept        := C_ACCEPT;
  FPIdentificador := C_IDENTIFICADOR;

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := OAuth.Ambiente = taHomologacao;
  end;

end;

function TBoletoW_Safra.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;
end;

function TBoletoW_Safra.Enviar: boolean;
begin
  Result := inherited Enviar;
end;

end.
