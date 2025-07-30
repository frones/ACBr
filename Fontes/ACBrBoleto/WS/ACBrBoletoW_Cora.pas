{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Luis Claudio Padilha Junior                    }
{                               Victor Hugo Gonzales - Pandaaa                 }
{                                                                              }
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
// incluido em:

unit ACBrBoletoW_Cora;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ACBrBoleto,
  ACBrBoletoConversao,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest.OAuth,
  ACBrBoletoWS.Rest,
  ACBrJSON;

type

  { TBoletoW_Cora }
  TBoletoW_Cora = class(TBoletoWSREST)
  private
    function CodigoTipoTitulo(AEspecieDoc: string): string;
    function DateIntertoDateTime(const AValue: string): TDateTime;
    function DateTimeToDateCora(const AValue: TDateTime): string;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    procedure DefinirIdentificador;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros: string;
    procedure DefinirParamOAuth; override;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoCancelar;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarServico(AJson: TACBrJSONObject);
    procedure GerarFormasPagamento(AJson: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GerarPagamento(AJson: TACBrJSONObject);
    procedure GeraDadosInstrucao(AJson: TACBrJSONObject);
    procedure AlteraDataVencimento(AJson: TACBrJSONObject);

    procedure AlteracaoDesconto(AJson: TACBrJSONObject);
    procedure AlteracaoDataDesconto(AJson: TACBrJSONObject);
    procedure AlterarProtesto(AJson: TACBrJSONObject);
    procedure AtribuirAbatimento(AJson: TACBrJSONObject);
    procedure AlteracaoAbatimento(AJson: TACBrJSONObject);
    procedure AtribuirJuros(AJson: TACBrJSONObject);
    procedure AtribuirMulta(AJson: TACBrJSONObject);
    procedure AtribuirNegativacao(AJson: TACBrJSONObject);
    procedure AlteracaoSeuNumero(AJson: TACBrJSONObject);
    procedure AlteracaoEnderecoPagador(AJson: TACBrJSONObject);
    procedure AlteracaoPrazo(AJson: TACBrJSONObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL             = 'https://matls-clients.api.cora.com.br/v2';
  C_URL_HOM         = 'https://matls-clients.api.stage.cora.com.br/v2';
  C_URL_OAUTH_PROD  = 'https://matls-clients.api.cora.com.br/token';
  C_URL_OAUTH_HOM   = 'https://matls-clients.api.stage.cora.com.br/token';
  C_CONTENT_TYPE    = 'application/json';
  C_ACCEPT          = C_CONTENT_TYPE;
  C_AUTHORIZATION   = 'Authorization';
  C_ACCEPT_ENCODING = '';
  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = C_CHARSET;

implementation

uses
   ACBrUtil.Strings,
   ACBrUtil.DateTime;

{ TBoletoW_Cora }

procedure TBoletoW_Cora.DefinirURL;
var
  LNossoNumeroCorrespondente: string;
begin

  if (ATitulo <> nil) then
    LNossoNumeroCorrespondente := ATitulo.NossoNumeroCorrespondente;

  case Boleto.Configuracoes.WebService.Ambiente of
    tawsProducao    : FPURL.URLProducao    := C_URL;
    tawsHomologacao : FPURL.URLHomologacao := C_URL_HOM;
  end;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      FPURL.SetPathURI( '/invoices' );
    tpConsulta:
      FPURL.SetPathURI( '/invoices?' + DefinirParametros );
    tpBaixa,tpCancelar:
      begin
        if (LNossoNumeroCorrespondente <> '') then
        begin
          FPURL.SetPathURI( '/invoices/' + LNossoNumeroCorrespondente );
        end;
      end;
    tpConsultaDetalhe:
      begin
        if (LNossoNumeroCorrespondente <> '') then
        begin
          FPURL.SetPathURI( '/invoices/' + LNossoNumeroCorrespondente );
        end;
      end;
  end;
end;

procedure TBoletoW_Cora.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Cora.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
  if Boleto.Configuracoes.WebService.Operacao = tpInclui then
    DefinirIdentificador;
end;

procedure TBoletoW_Cora.GerarDados;
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
        raise EACBrBoletoWSException.Create
          (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
          [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
      end;
    tpCancelar, tpBaixa:
      begin
        FMetodoHTTP := htDELETE; // Define Método POST para Baixa
        RequisicaoCancelar;
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

procedure TBoletoW_Cora.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

procedure TBoletoW_Cora.DefinirIdentificador;
var
  key: TGUID;
  key_text: string;
begin

  if Assigned(ATitulo) then
  begin
    CreateGUID(key);
    key_text := GUIDToString(key);
    key_text := StringReplace(key_text, '{', '', [rfReplaceAll]);
    key_text := StringReplace(key_text, '}', '', [rfReplaceAll]);

    FPIdentificador := 'Idempotency-Key: ' + key_text;
  end;

end;

function TBoletoW_Cora.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := true;
  OAuth.AuthorizationType := atNoAuth;

  Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Cora.RequisicaoCancelar;
begin
  //
end;

procedure TBoletoW_Cora.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Cora.DefinirParametros: string;
var
  Consulta: TStringList;
  Documento: string;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    Documento := OnlyNumber
      (Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

    Consulta := TStringList.Create;
    Consulta.Delimiter := '&';
    try

      case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
        isbBaixado:
          begin
            if (Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio = 0) or
               (Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal = 0) then
             raise EACBrBoletoWSException.Create
             ('Para consulta isbBaixado, utilizar no filtro:'+sLineBreak+
             'dataMovimento.DataInicio e dataMovimento.DataFinal');

            // possivel apenas um estado por vez.
            Consulta.Add('start=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio));
            Consulta.Add('end=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal));
            Consulta.Add('state=PAID');
          end;
        isbCancelado:
          begin
            if (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio = 0) or
               (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal = 0) then
             raise EACBrBoletoWSException.Create
             ('Para consulta isbCancelado, utilizar no filtro:'+sLineBreak+
             'dataVencimento.DataInicio e dataVencimento.DataFinal');

            // possivel apenas um estado por vez.
            Consulta.Add('start=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataVencimento.
              DataInicio));
            Consulta.Add('end=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal));
            Consulta.Add('state=CANCELLED');
          end;
        isbAberto:
          begin
            if (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio = 0) or
               (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal = 0) then
             raise EACBrBoletoWSException.Create
             ('Para consulta isbAberto, utilizar no filtro:'+sLineBreak+
             'dataVencimento.DataInicio e dataVencimento.DataFinal');

            // possivel apenas por datavencimento

              Consulta.Add('start=' + DateTimeToDateCora
                (Boleto.Configuracoes.WebService.Filtro.dataVencimento.
                DataInicio));
              Consulta.Add('end=' + DateTimeToDateCora
                (Boleto.Configuracoes.WebService.Filtro.dataVencimento.
                DataFinal));
              Consulta.Add('state=OPEN');

          end;
        isbNenhum:
          begin
            if (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio <= 0) or
               (Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal <= 0) then
             raise EACBrBoletoWSException.Create
             ('Para consulta isbNenhum, utilizar no filtro:'+sLineBreak+
             'dataVencimento.DataInicio e dataVencimento.DataFinal');


            // caso queira tudo, não indicar o estado
            Consulta.Add('start=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataVencimento.
              DataInicio));
            Consulta.Add('end=' + DateTimeToDateCora
              (Boleto.Configuracoes.WebService.Filtro.dataVencimento.
              DataFinal));
          end;
      end;

      Consulta.Add('perPage=100');

      if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
        Consulta.Add('page=' +
          FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade))
      else
        Consulta.Add('page=1');

    finally
      Result := Consulta.DelimitedText;
      Consulta.Free;
    end;

  end;

end;

procedure TBoletoW_Cora.DefinirParamOAuth;
begin
  FParamsOAuth := Format('client_id=%s&grant_type=client_credentials',
    [Boleto.Cedente.CedenteWS.ClientID]);
  OAuth.ExigirClientSecret := False;
end;

function TBoletoW_Cora.DateIntertoDateTime(const AValue: string): TDateTime;
begin
  Result := StrToDateDef(StringReplace(AValue, '.', '/', [rfReplaceAll]), 0);
end;

function TBoletoW_Cora.DateTimeToDateCora(const AValue: TDateTime): string;
begin
  Result := FormatDateBr(AValue, 'YYYY-MM-DD');
end;

procedure TBoletoW_Cora.DefinirAutenticacao;
begin
end;

function TBoletoW_Cora.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(
              IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao, '1','2')
            , 2);
end;

procedure TBoletoW_Cora.RequisicaoJson;
var
  LJSON: TACBrJSONObject;
  LSeuNumero: string;
begin
  if Assigned(ATitulo) then
  begin
    LJSON := TACBrJSONObject.Create;
    try

      if not ((ATitulo.NossoNumero = '0') or (ATitulo.NossoNumero = Poem_Zeros('',Boleto.Banco.TamanhoMaximoNossoNum)) ) then
        raise Exception.Create('Campo NossoNumero é inválido obrigatóriamente deve ser informado valor 0!');

      LSeuNumero := Trim(ATitulo.SeuNumero);

      if (LSeuNumero <> '') then
        LJSON.AddPair('code', LSeuNumero);

      GerarPagador(LJSON);
      GerarPagamento(LJSON);
      GerarServico(LJSON);
      GerarFormasPagamento(LJSON);

      FPDadosMsg := LJSON.ToJSON;

    finally
      LJSON.Free;
    end;
  end;
end;

procedure TBoletoW_Cora.RequisicaoAltera;
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.GeraDadosInstrucao(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.RequisicaoConsulta;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Cora.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Cora.GerarServico(AJson: TACBrJSONObject);
var
  LJSON: TACBrJSONObject;
  LJSONArray : TACBrJSONArray;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJSONArray := TACBrJSONArray.Create;
    LJSON := TACBrJSONObject.Create;

    LJSON.AddPair('name', ATitulo.NumeroDocumento);
    LJSON.AddPair('description', ATitulo.Detalhamento.Text);

    LJSON.AddPair('amount', ATitulo.ValorDocumento * 100);

    LJSONArray.AddElementJSON(LJSON);

    AJson.AddPair('services',LJSONArray);
  end;
end;

procedure TBoletoW_Cora.GerarPagador(AJson: TACBrJSONObject);
var
  LJSONPagador: TACBrJSONObject;
  LJSONEndereco: TACBrJSONObject;
  LJSONDocumento: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJSONPagador := TACBrJSONObject.Create;
    LJSONDocumento := TACBrJSONObject.Create;
    LJSONEndereco := TACBrJSONObject.Create;

    LJSONPagador.AddPair('name', ATitulo.Sacado.NomeSacado);
    LJSONPagador.AddPair('email', ATitulo.Sacado.Email);

    LJSONEndereco.AddPair('street', ATitulo.Sacado.Logradouro);
    LJSONEndereco.AddPair('number', ATitulo.Sacado.Numero);
    LJSONEndereco.AddPair('complement', ATitulo.Sacado.Complemento);
    LJSONEndereco.AddPair('district', ATitulo.Sacado.Bairro);
    LJSONEndereco.AddPair('zip_code', OnlyNumber(ATitulo.Sacado.CEP));
    LJSONEndereco.AddPair('city', ATitulo.Sacado.Cidade);
    LJSONEndereco.AddPair('state', ATitulo.Sacado.UF);

    LJSONDocumento.AddPair('identity', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    LJSONDocumento.AddPair('type', IfThen(Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'CPF', 'CNPJ'));

    LJSONPagador.AddPair('document',LJSONDocumento);
    LJSONPagador.AddPair('address',LJSONEndereco);

    AJson.AddPair('customer',LJSONPagador);

  end;
end;

procedure TBoletoW_Cora.GerarPagamento(AJson: TACBrJSONObject);
var
  LJSON: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJSON := TACBrJSONObject.Create;

    LJSON.AddPair('due_date', DateTimeToDateCora(ATitulo.Vencimento));
    GerarJuros(LJSON);
    GerarMulta(LJSON);
    GerarDesconto(LJSON);
    AJson.AddPair('payment_terms', LJSON);
  end;
end;

procedure TBoletoW_Cora.GerarBenificiarioFinal(AJson: TACBrJSONObject);
var
  LJSON : TACBrJSONObject;
begin

  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.Sacado.SacadoAvalista.CNPJCPF <> EmptyStr) then
  begin

    LJSON := TACBrJSONObject.Create;

    LJSON.AddPair('nome', ATitulo.Sacado.SacadoAvalista.NomeAvalista);
    LJSON.AddPair('cpfCnpj', OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF));
    LJSON.AddPair('tipoPessoa', IfThen(Length(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,
                                               'FISICA', 'JURIDICA'));
    LJSON.AddPair('cep', ATitulo.Sacado.SacadoAvalista.CEP);
    LJSON.AddPair('endereco', ATitulo.Sacado.SacadoAvalista.Logradouro);
    LJSON.AddPair('bairro', ATitulo.Sacado.SacadoAvalista.Bairro);
    LJSON.AddPair('cidade', ATitulo.Sacado.SacadoAvalista.Cidade);
    LJSON.AddPair('uf', ATitulo.Sacado.SacadoAvalista.UF);

    AJson.AddPair('beneficiarioFinal', LJSON);
  end;
end;

procedure TBoletoW_Cora.GerarJuros(AJson: TACBrJSONObject);
var
  LJSON : TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    if (ATitulo.ValorMoraJuros > 0) then
    begin
      LJSON := TACBrJSONObject.Create;
      if ATitulo.CodigoMora = '' then
      begin
        case ATitulo.CodigoMoraJuros of
          cjValorDia   : ATitulo.CodigoMora := '1';
          cjTaxaMensal : ATitulo.CodigoMora := '2';
          cjIsento     : ATitulo.CodigoMora := '3';
        end;
      end;

      case (StrToIntDef(ATitulo.CodigoMora, 3)) of
        1: LJSON.AddPair('rate', ATitulo.ValorMoraJuros);
        2: LJSON.AddPair('rate', ATitulo.ValorMoraJuros);
      end;
      if LJSON <> nil then
        AJson.AddPair('interest', LJSON);
    end;
  end;
end;

procedure TBoletoW_Cora.GerarMulta(AJson: TACBrJSONObject);
var
  LJSON: TACBrJSONObject;
  LCodigoMulta: Byte;
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.DataMulta > 0) then
  begin
    LJSON := TACBrJSONObject.Create;
    if ATitulo.PercentualMulta > 0 then
    begin
      if ATitulo.MultaValorFixo then
        LCodigoMulta := 1
      else
        LCodigoMulta := 2;
    end
    else
      LCodigoMulta := 3;

    case LCodigoMulta of
      1:
        begin
          LJSON.AddPair('date', DateTimeToDateCora(ATitulo.DataMulta));
          LJSON.AddPair('amount', ATitulo.PercentualMulta * 100);
        end;
      2:
        begin
          LJSON.AddPair('date', DateTimeToDateCora(ATitulo.DataMulta));
          LJSON.AddPair('rate', ATitulo.PercentualMulta);
        end;
    end;

    AJson.AddPair('fine', LJSON);
  end;
end;

procedure TBoletoW_Cora.GerarDesconto(AJson: TACBrJSONObject);
var
  LJSON: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.DataDesconto > 0) then
  begin
    LJSON := TACBrJSONObject.Create;
    LJSON.AddPair('data', DateTimeToDateCora(ATitulo.DataDesconto));
    case Integer(ATitulo.TipoDesconto) of
      1:
        begin
          LJSON.AddPair('type', 'FIXED');
          LJSON.AddPair('value', ATitulo.ValorDesconto * 100);
        end;
      2:
        begin
          LJSON.AddPair('type', 'PERCENT');
          LJSON.AddPair('value', ATitulo.ValorDesconto);
        end;
    end;
    AJson.AddPair('discount',LJSON);
  end;
end;

procedure TBoletoW_Cora.GerarFormasPagamento(AJson: TACBrJSONObject);
var
  LJSON : TACBrJSONArray;
begin
  if Assigned(ATitulo) and  Assigned(AJson) then
  begin
    LJSON := TACBrJSONArray.Create;
    LJSON.AddElement('BANK_SLIP');

    if Boleto.Cedente.CedenteWS.IndicadorPix THEN
      LJSON.AddElement('PIX');

    AJson.AddPair('payment_forms', LJSON);
  end;
end;

procedure TBoletoW_Cora.AlteraDataVencimento(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoDesconto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoDataDesconto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlterarProtesto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AtribuirAbatimento(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoAbatimento(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AtribuirJuros(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AtribuirMulta(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AtribuirNegativacao(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoSeuNumero(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoEnderecoPagador(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Cora.AlteracaoPrazo(AJson: TACBrJSONObject);
begin
  // sem Payload
end;

function TBoletoW_Cora.CodigoTipoTitulo(AEspecieDoc: string): string;
begin
end;

constructor TBoletoW_Cora.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    case OAuth.Ambiente of
      tawsProducao: OAuth.URL.URLProducao := C_URL_OAUTH_PROD;
      tawsHomologacao: OAuth.URL.URLHomologacao := C_URL_OAUTH_HOM;
    end;

    OAuth.Payload := True;
  end;

end;

function TBoletoW_Cora.GerarRemessa: string;
begin
  DefinirCertificado;
  Result := inherited GerarRemessa;
end;

function TBoletoW_Cora.Enviar: boolean;
begin
  DefinirCertificado;
  Result := inherited Enviar;
end;

end.
