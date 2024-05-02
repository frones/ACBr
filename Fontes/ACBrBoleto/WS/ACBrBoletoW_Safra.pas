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
  ACBrBoletoWS.Rest,
  ACBrJSON;

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
    procedure GerarDocumento(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GerarMultaJuros(AJson: TACBrJsonObject);
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarEnderecoPagador(AJson: TACBrJSONObject);
    procedure GeraMensagem(AJson: TACBrJsonObject);

    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);

    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;

    procedure GeraDadosInstrucao(AJson: TACBrJSONObject);
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
  LData: string;
  LJson: TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    LJson := TACBrJSONObject.Create;
    try
      LJson.AddPair('agencia',IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Agencia, ''));
      LJson.AddPair('conta', IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, aTitulo.ACBrBoleto.Cedente.Conta + aTitulo.ACBrBoleto.Cedente.ContaDigito,''));
      GerarDocumento(LJson);
      FPDadosMsg := LJson.ToJSON;
    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarDocumento(AJson: TACBrJSONObject);
const
  EPC_DUPLICATA_MERCANTIL  = '01';
  EPC_NOTA_PROMISSORIA     = '02';
  EPC_NOTA_DE_SEGURO       = '03';
  EPC_RECIBO               = '05';
  EPC_DUPLICATA_DE_SERVICO = '09';
var
  LJsonDocumento, LJsonPairDocumento  : TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDocumento := TACBrJSONObject.Create;
      LJsonDocumento.AddPair('numero', Copy(OnlyNumber(aTitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(aTitulo)), 1, 15));
      LJsonDocumento.AddPair('numeroCliente', Copy(aTitulo.SeuNumero, 1, 10));
      LJsonDocumento.AddPair('diasDevolucao', 90);
      LJsonDocumento.AddPair('especie', EPC_DUPLICATA_MERCANTIL);
      LJsonDocumento.AddPair('dataVencimento', DateTimeToDateInter(aTitulo.Vencimento));
      LJsonDocumento.AddPair('valor', aTitulo.ValorDocumento);
      LJsonDocumento.AddPair('codigoMoeda', 0);
      LJsonDocumento.AddPair('quantidadeDiasProtesto', aTitulo.DiasDeProtesto);
      LJsonDocumento.AddPair('campoLivre', Copy('', 1, 25));
      LJsonDocumento.AddPair('fidc', 0);
      LJsonDocumento.AddPair('danfe', Copy('', 1, 44));
      LJsonDocumento.AddPair('valorAbatimento', aTitulo.ValorAbatimento);
        //            LJsonDocumento.Add('identificacaoAceite').Value.AsString     := aTitulo.Aceite.atSim;

      GerarDesconto(LJsonDocumento);
      GerarMultaJuros(LJsonDocumento);
      GerarPagador(LJsonDocumento);
      GeraMensagem(LJsonDocumento);

      AJson.AddPair('Documento',LJsonDocumento);
    end;
  end;
end;

procedure TBoletoW_Safra.GerarDesconto(AJson: TACBrJSONObject);
Const
  C_DESCONTO_EM_VALOR                                    = 1;
  C_DESCONTO_EM_PERCENTUAL                               = 2;
  C_DESCONTO_EM_VALOR_POR_ANTECIPACAO_DIAS_CORRIDOS      = 3;
  C_DESCONTO_EM_PERCENTUAL_POR_ANTECIPACAO_DIAS_CORRIDOS = 5;
var
  LJsonDesconto : TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (aTitulo.ValorDesconto + aTitulo.ValorDesconto2 + aTitulo.ValorDesconto3) > 0 then
      begin
        LJsonDesconto := TACBrJSONObject.Create;
        if (aTitulo.DataDesconto > 0) then
        begin
          LJsonDesconto.AddPair('data', DateTimeToDateInter(aTitulo.DataDesconto));
          LJsonDesconto.AddPair('valor', RoundABNT(aTitulo.ValorDesconto, 2));
        end;

        if (aTitulo.DataDesconto2 > 0) then
        begin
          LJsonDesconto.AddPair('data2', DateTimeToDateInter(aTitulo.DataDesconto2));
          LJsonDesconto.AddPair('valor2', RoundABNT(aTitulo.ValorDesconto2, 2));
        end;

        if (aTitulo.DataDesconto3 > 0) then
        begin
          LJsonDesconto.AddPair('data3', DateTimeToDateInter(aTitulo.DataDesconto3));
          LJsonDesconto.AddPair('valor3', RoundABNT(aTitulo.ValorDesconto3, 2));
        end;

        LJsonDesconto.AddPair('tipoDesconto ', C_DESCONTO_EM_VALOR);

        AJson.AddPair('desconto',LJsonDesconto);
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarMultaJuros(AJson: TACBrJsonObject);
var
  LJsonMultaJuros : TACBrJsonObject;
  LMulta            : Double;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (aTitulo.PercentualMulta + aTitulo.ValorMoraJuros) > 0 then
      begin
        LJsonMultaJuros := TACBrJSONObject.Create;
        LJsonMultaJuros.AddPair('dataMulta', DateTimeToDateInter(aTitulo.DataMulta));
        if aTitulo.MultaValorFixo then
          LMulta := RoundABNT((aTitulo.PercentualMulta * 100) / aTitulo.ValorDocumento, 2)
        else
          LMulta := aTitulo.PercentualMulta;

        LJsonMultaJuros.AddPair('taxaMulta', aTitulo.PercentualMulta);

        LJsonMultaJuros.AddPair('dataJuros', DateTimeToDateInter(aTitulo.DataMulta));

        LJsonMultaJuros.AddPair('taxaJuros', aTitulo.ValorMoraJuros);

        AJson.AddPair('multa',LJsonMultaJuros);
      end;
    end;
  end;
end;

procedure TBoletoW_Safra.GerarPagador(AJson: TACBrJSONObject);
var
  LJsonDadosPagador: TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDadosPagador := TACBrJSONObject.Create;
      LJsonDadosPagador.AddPair('nome', Copy(aTitulo.Sacado.NomeSacado, 1, 40));
      LJsonDadosPagador.AddPair('tipoPessoa', IfThen(Length(OnlyNumber(aTitulo.Sacado.CNPJCPF)) = 11, 'F', 'J'));
      LJsonDadosPagador.AddPair('numeroDocumento', OnlyNumber(aTitulo.Sacado.CNPJCPF));
      LJsonDadosPagador.AddPair('email',Copy(aTitulo.Sacado.Email, 1, 14));
      GerarEnderecoPagador(LJsonDadosPagador);
      AJson.AddPair('pagador',LJsonDadosPagador);
    end;
  end;
end;

procedure TBoletoW_Safra.GerarEnderecoPagador(AJson: TACBrJsonObject);
var
  LJsonDadosPagador: TACBrJSONObject;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonDadosPagador := TACBrJSONObject.Create;
      LJsonDadosPagador.AddPair('logradouro', Copy(aTitulo.Sacado.Logradouro + ', ' + aTitulo.Sacado.Numero + ', ' + aTitulo.Sacado.Complemento, 1, 40));
      LJsonDadosPagador.AddPair('bairro', Copy(aTitulo.Sacado.Bairro, 1, 10));
      LJsonDadosPagador.AddPair('cidade', Copy(aTitulo.Sacado.Cidade, 1, 15));
      LJsonDadosPagador.AddPair('uf', aTitulo.Sacado.UF);
      LJsonDadosPagador.AddPair('cep', OnlyNumber(aTitulo.Sacado.CEP));
      AJson.AddPair('endereco',LJsonDadosPagador);
    end;
  end;
end;

procedure TBoletoW_Safra.GeraMensagem(AJson: TACBrJsonObject);
Const
  TP_MENSAGEM_RECIBO = 1;
  TP_MENSAGEM_FICHA  = 2;
var
  LJsonMensagem : TACBrJSONObject;
  LJsonArrayMensagem: TACBrJSONArray;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      LJsonArrayMensagem := TACBrJSONArray.Create;
      LJsonMensagem      := TACBrJSONObject.Create;
      LJsonMensagem.AddPair('posicao', TP_MENSAGEM_RECIBO);
      LJsonMensagem.AddPair('mensagem', Copy('', 1, 72));
      AJson.AddPair('mensagem',LJsonMensagem);
    end;
  end;
end;

procedure TBoletoW_Safra.GeraDadosInstrucao(AJson: TACBrJSONObject);
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

procedure TBoletoW_Safra.GerarBenificiarioFinal(AJson: TACBrJSONObject);
var
  LJsonSacadorAvalista, LJsonPairSacadorAvalista    : TACBrJsonObject;
begin
  if Assigned(aTitulo) then
  begin
    if aTitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    if Assigned(AJson) then
    begin
      LJsonSacadorAvalista := TACBrJSONObject.Create;
      LJsonSacadorAvalista.AddPair('nome', aTitulo.Sacado.SacadoAvalista.NomeAvalista);
      LJsonSacadorAvalista.AddPair('cpfCnpj', OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF));
      LJsonSacadorAvalista.AddPair('tipoPessoa',IfThen(Length(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, 'FISICA', 'JURIDICA'));
      LJsonSacadorAvalista.AddPair('cep', aTitulo.Sacado.SacadoAvalista.CEP);
      LJsonSacadorAvalista.AddPair('endereco', aTitulo.Sacado.SacadoAvalista.Logradouro);
      LJsonSacadorAvalista.AddPair('bairro', aTitulo.Sacado.SacadoAvalista.Bairro);
      LJsonSacadorAvalista.AddPair('cidade', aTitulo.Sacado.SacadoAvalista.Cidade);
      LJsonSacadorAvalista.AddPair('uf', aTitulo.Sacado.SacadoAvalista.UF);
      AJson.AddPair('beneficiarioFinal',LJsonSacadorAvalista);
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
