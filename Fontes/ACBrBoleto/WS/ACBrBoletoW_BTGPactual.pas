{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Panda                       }
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

//incluido em::

{$I ACBr.inc}

unit ACBrBoletoW_BTGPactual;

interface

uses
  ACBrJSON,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest;

type

  { TBoletoW_BTGPactual }
  TBoletoW_BTGPactual = class(TBoletoWSREST)
  private

  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    procedure DefinirParamOAuth; override;
    procedure RequisicaoIncluir;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure RequisicaoBaixa;
    procedure RequisicaoAltera;
    procedure RequisicaoAlterarDesconto(AJson: TACBrJSONObject);
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);

    function GerarTokenAutenticacao: string; override;
    function DefinirParametros:String;
  public
    constructor Create(ABoletoWS: TBoletoWS); override;

  end;
const
  VERSAO_API = 'v1.4';

  {URL Produção}
  C_URL                   = 'https://api.empresas.btgpactual.com/v1/bank-slips';
  C_URL_OAUTH_PROD        = 'https://id.btgpactual.com/oauth2/token';

  {URL Homologação}
  C_URL_HOMOLOGACAO       = 'https://api.sandbox.empresas.btgpactual.com/v1/bank-slips';
  C_URL_OAUTH_HOMOLOGACAO = 'https://id.sandbox.btgpactual.com/oauth2/token';

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  pcnConversao,
  ACBrBoletoConversao,
  ACBrPixBase,
  ACBrUtil.Strings,
  ACBrBoletoRet_Cora;

{ TBoletoW_BTGPactual }

procedure TBoletoW_BTGPactual.DefinirURL;
var LBankSlipID, LConvenio : String;
begin
  LConvenio := '/?accountId=' + Boleto.Cedente.Convenio;

  if Assigned(ATitulo) then
    LBankSlipID := ATitulo.NossoNumeroCorrespondente;

  case Boleto.Configuracoes.WebService.Ambiente of
    tawsProducao    : FPURL := C_URL;
    tawsHomologacao : FPURL := C_URL_HOMOLOGACAO;
    tawsSandBox     : FPURL := C_URL_HOMOLOGACAO;
  end;

  case Boleto.Configuracoes.WebService.Operacao of
     tpInclui         : FPURL := FPURL + LConvenio;
     tpAltera         : FPURL := FPURL + LBankSlipID + LConvenio;
     tpConsulta,
     tpConsultaDetalhe: FPURL := FPURL + LConvenio + DefinirParametros;
     tpBaixa,
     tpCancelar       : FPURL := FPURL + LBankSlipID + LConvenio;
  end;
end;

procedure TBoletoW_BTGPactual.DefinirContentType;
begin
  FPContentType := 'application/json';
end;

procedure TBoletoW_BTGPactual.GerarHeader;
begin
  ClearHeaderParams;
  AddHeaderParam('Accept-Encoding', 'gzip, deflate, compress');
  DefinirContentType;
end;

procedure TBoletoW_BTGPactual.GerarDados;
begin
  if Assigned(ATitulo) then
  begin
    case Boleto.Configuracoes.WebService.Operacao of
      tpInclui:
        begin
          FMetodoHTTP := htPOST;
          RequisicaoIncluir;
        end;
      tpAltera:
        begin
          FMetodoHTTP := htPUT; // Define Método PUT conforme manual do banco
          RequisicaoAltera;
        end;
      tpBaixa,
      tpCancelar :
        begin
          FMetodoHTTP := htDELETE; // Define Método DELETE conforme manual do banco
          RequisicaoBaixa;
        end;
    else
      raise EACBrBoletoWSException.Create(ClassName + Format( S_OPERACAO_NAO_IMPLEMENTADO, [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
    end;
  end else
  if Boleto.Configuracoes.WebService.Operacao = tpConsulta then
  begin
    FMetodoHTTP := htGET; // Define Método GET conforme manual do banco
    RequisicaoConsulta;
  end else
    raise EACBrBoletoWSException.Create(ClassName + Format( S_OPERACAO_NAO_IMPLEMENTADO, [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
end;

procedure TBoletoW_BTGPactual.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_BTGPactual.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
end;

function TBoletoW_BTGPactual.DefinirParametros: String;
var
  LConsultaList: TStringList;
  LNossoNumero: String;
begin
  case Boleto.Configuracoes.WebService.Operacao of
    tpConsulta:
      begin
        if Assigned(Boleto.Configuracoes.WebService.Filtro) then
        begin
          LConsultaList := TStringList.Create;
          try
            LConsultaList.Delimiter := '?';
            LConsultaList.Add('limit='+'1000' );

            if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
              LConsultaList.Add('limit='+ FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade));

            case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
              isbBaixado: LConsultaList.Add('status='+'PAID');
              isbAberto:  LConsultaList.Add('status='+'CREATED');
              isbCancelado: LConsultaList.Add('status='+'CANCELED');
            end;
            //DataVencimento
            if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
              begin
              LConsultaList.Add('startDueDate=' +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio));
              LConsultaList.Add('endDueDate='   +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal));
              end;
            //DataRegistro
            if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0 then
              begin
              LConsultaList.Add('startDate=' +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio));
              LConsultaList.Add('endDate='   +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal));
              end;
            //DataPagamento
            if Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio > 0 then
              begin
              LConsultaList.Add('startPaymentDate=' +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio));
              LConsultaList.Add('endPaymentDate='   +DateToStr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal));
              end;
          finally
            Result := LConsultaList.DelimitedText;
            LConsultaList.Free
          end;
        end;
      end;
    tpConsultaDetalhe:
      begin
        if Assigned(ATitulo) then
        begin
          LNossoNumero := ATitulo.NossoNumero;
        end;
      end;
  end;
end;

procedure TBoletoW_BTGPactual.DefinirParamOAuth;
begin
  FParamsOAuth := C_GRANT_TYPE
                + '=authorization_code'
                + '&response_type=code'
                + '&client_id=' + Boleto.Cedente.CedenteWS.ClientID
                + '&prompt=login HTTP/1.1';
end;

procedure TBoletoW_BTGPactual.RequisicaoAltera;
var
  LJsonObject, LJsonObjectInterests: TACBrJSONObject;
begin
 if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('amount', ATitulo.ValorDocumento);
      LJsonObject.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', ATitulo.Vencimento));
      LJsonObjectInterests := TACBrJSONObject.Create;
      GerarJuros(LJsonObjectInterests);
      GerarMulta(LJsonObjectInterests);

      LJsonObject.AddPair('interests', LJsonObjectInterests); //Juros e Multas
      
      RequisicaoAlterarDesconto(LJsonObject);
      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BTGPactual.RequisicaoBaixa;
begin
  // Sem Payload
end;

procedure TBoletoW_BTGPactual.RequisicaoConsulta;
begin
  // Sem Payload
end;

procedure TBoletoW_BTGPactual.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_BTGPactual.RequisicaoIncluir;
var
  LJsonObject, LJsonObjectInterests: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('amount', ATitulo.ValorDocumento);
      LJsonObject.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', ATitulo.Vencimento));
      if ATitulo.DataLimitePagto > 0 then
        LJsonObject.AddPair('overDueDate', FormatDateTime('yyyy-mm-dd', ATitulo.DataLimitePagto));
      LJsonObject.AddPair('referenceNumber', ATitulo.SeuNumero);
      if Trim(ATitulo.Mensagem.Text) <> '' then
        LJsonObject.AddPair('description', Trim(ATitulo.Mensagem.Text));

      GerarPagador(LJsonObject);

      if ATitulo.Parcela <= 0 then
        ATitulo.Parcela := 1;

      LJsonObject.AddPair('installments', ATitulo.Parcela);

      LJsonObjectInterests := TACBrJSONObject.Create;
      GerarJuros(LJsonObjectInterests);
      GerarMulta(LJsonObjectInterests);

      LJsonObject.AddPair('interests', LJsonObjectInterests); //Juros e Multas

      if ATitulo.ValorDesconto > 0 then
         GerarDesconto(LJsonObject);

      LJsonObject.AddPair('ourNumber', ATitulo.SeuNumero);
      LJsonObject.AddPair('pixInfo', TACBrJSONObject.Create.
                                       AddPair('generateEmbeddedQrCode',ATitulo.ACBrBoleto.Cedente.CedenteWS.IndicadorPix)
                         );

      FPDadosMsg := LJsonObject.ToJson;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BTGPactual.GerarPagador(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
  LCEP: string;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonObject := TACBrJSONObject.Create;

    LJsonObject.AddPair('name', Trim(ATitulo.Sacado.NomeSacado));
    LJsonObject.AddPair('taxId', OnlyNumber(ATitulo.Sacado.CNPJCPF));

    LCEP := ACBrUtil.Strings.PadLeft(OnlyNumber(ATitulo.Sacado.CEP), 8, '0');
    LJsonObject.AddPair('address', TACBrJSONObject.Create
                                     .AddPair('zipCode', Copy(LCEP, 1, 5) + '-' + Copy(LCEP, 6, 3))
                                     .AddPair('city', ATitulo.Sacado.Cidade)
                                     .AddPair('state', ATitulo.Sacado.UF)
                                     .AddPair('street', ATitulo.Sacado.Logradouro)
                                     .AddPair('number', ATitulo.Sacado.Numero)
                       );
    AJson.AddPair('payer',LJsonObject);
  end;
end;

procedure TBoletoW_BTGPactual.GerarMulta(AJson: TACBrJSONObject);
var LType : String;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LType := 'NOT_APPLICABLE';
    if ATitulo.MultaValorFixo then
      LType := 'FIXED_VALUE'
    else
      LType := 'PERCENTAGE';

    if ATitulo.PercentualMulta > 0 then
    begin
      AJson.AddPair('value', ATitulo.PercentualMulta);
      AJson.AddPair('type', LType);
    end else
      AJson.AddPair('value', '0.00');
    AJson.AddPair('type', LType);
  end;
end;

procedure TBoletoW_BTGPactual.GerarJuros(AJson: TACBrJSONObject);
var LType : String;
begin
  case ATitulo.CodigoMoraJuros of
    cjTaxaMensal,
    cjTaxaDiaria   : LType := 'PERCENTAGE';
    cjIsento       : LType := 'NOT_APPLICABLE';
    cjValorMensal,
    cjValorDia     : LType := 'FIXED_VALUE';
  end;

  AJson.AddPair('arrears', TACBrJSONObject.Create
                             .AddPair('value', ATitulo.ValorMoraJuros)
                             .AddPair('type', LType)
               );
end;

procedure TBoletoW_BTGPactual.RequisicaoAlterarDesconto(AJson: TACBrJSONObject);
var
  LJsonArrayDesconto : TACBrJSONArray;
begin
  LJsonArrayDesconto := TACBrJSONArray.Create;

  if (ATitulo.DataDesconto  > 0) or
     (ATitulo.DataDesconto2 > 0) or
     (ATitulo.DataDesconto3 > 0) then
  begin
    if (ATitulo.DataDesconto  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto))
                                       );
    if (ATitulo.DataDesconto2  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto2)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto2))
                                       );
    if (ATitulo.DataDesconto3  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto3)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto3))
                                       );
  end;
  AJson.AddPair('discounts',LJsonArrayDesconto);
end;

procedure TBoletoW_BTGPactual.GerarDesconto(AJson: TACBrJSONObject);
var
  LJsonArrayDesconto : TACBrJSONArray;
begin
  if (ATitulo.DataDesconto  > 0) or
     (ATitulo.DataDesconto2 > 0) or
     (ATitulo.DataDesconto3 > 0) then
  begin
    LJsonArrayDesconto := TACBrJSONArray.Create;

    if (ATitulo.DataDesconto  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto))
                                       );
    if (ATitulo.DataDesconto2  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto2)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto2))
                                       );
    if (ATitulo.DataDesconto3  > 0) then
      LJsonArrayDesconto.AddElementJSON(TACBrJSONObject.Create
                                          .AddPair('value', ATitulo.ValorDesconto3)
                                          .AddPair('type', 'FIXED_VALUE')
                                          .AddPair('dueDate',FormatDateTime('yyyy-mm-dd', ATitulo.DataDesconto3))
                                       );
    AJson.AddPair('discounts',LJsonArrayDesconto);
  end;
end;

constructor TBoletoW_BTGPactual.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
  FPAccept := '';

  if Assigned(OAuth) then
  begin
    case OAuth.Ambiente of
      tawsProducao    : OAuth.URL := C_URL_OAUTH_PROD;
      tawsHomologacao : OAuth.URL := C_URL_OAUTH_HOMOLOGACAO;
      tawsSandBox     : OAuth.URL := C_URL_OAUTH_HOMOLOGACAO;
    end;

    OAuth.Payload := True;
    OAuth.ExigirClientSecret := False;
  end;
end;

end.
