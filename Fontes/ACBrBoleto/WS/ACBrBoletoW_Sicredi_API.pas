{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Leandro do Couto,}
{  Fernando Henrique                                                           }
{                                                                              }
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

unit ACBrBoletoW_Sicredi_API;

interface

uses
  Classes, SysUtils, ACBrBoletoWS, pcnConversao, ACBrBoletoConversao,
  synacode, strutils, DateUtils, ACBrDFeSSL, synautil, ACBrBoleto,
  Jsons;

type

  { TBoletoW_Sicredi_API }
  TBoletoW_Sicredi_API = class(TBoletoWSREST)
  private
    FDFeSSL:TDFeSSL;
    function CodigoTipoTitulo(AEspecieDoc:String): String;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros:String;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarBenificiarioFinal(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GeraDadosInstrucao(AJson:TJsonObject );

    procedure AlteraDataVencimento( AJson: TJsonObject);
    procedure AtribuirDesconto(AJson: TJsonObject);
    procedure AlteracaoDesconto(AJson: TJsonObject);
    procedure AlteracaoDataDesconto(AJson: TJsonObject);
    procedure AlterarProtesto(AJson: TJsonObject);
    procedure AtribuirAbatimento(AJson: TJsonObject);
    procedure AlteracaoAbatimento(AJson: TJsonObject);
    procedure AtribuirJuros(AJson: TJsonObject);
    procedure AtribuirMulta(AJson: TJsonObject);
    procedure AtribuirNegativacao(AJson: TJsonObject);
    procedure AlteracaoSeuNumero(AJson: TJsonObject);
    procedure AlteracaoEnderecoPagador(AJson: TJsonObject);
    procedure AlteracaoPrazo(AJson: TJsonObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL            = 'https://cobrancaonline.sicredi.com.br/sicredi-cobranca-ws-ecomm-api/ecomm/v1/boleto';
  C_URL_HOM        = 'https://cobrancaonline.sicredi.com.br/sicredi-cobranca-ws-ecomm-api/ecomm/v1/boleto';

  C_URL_OAUTH_PROD = 'https://cobrancaonline.sicredi.com.br/sicredi-cobranca-ws-ecomm-api/ecomm/v1/boleto/autenticacao';
  C_URL_OAUTH_HOM  = 'https://cobrancaonline.sicredi.com.br/sicredi-cobranca-ws-ecomm-api/ecomm/v1/boleto/autenticacao';


  C_CONTENT_TYPE   = 'application/json';
  C_ACCEPT         = 'application/json';
  C_AUTHORIZATION  = '';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'utf-8';

  C_ACCESS_TOKEN    = 'token';

implementation
	uses
   		ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;


{ TBoletoW_Sicredi_API }

procedure TBoletoW_Sicredi_API.DefinirURL;
var
aNossoNumero  : string;
begin

  aNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,C_URL, C_URL_HOM);
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui   : FPURL := FPURL + '/emissao';
    tpConsulta : FPURL := FPURL + '/consulta?agencia='+Boleto.Cedente.Agencia
                                + '&posto='+Boleto.Cedente.AgenciaDigito
                                + '&cedente='+Boleto.Cedente.CodigoCedente
                                + '&'
                                + DefinirParametros;
    tpAltera   : FPURL := FPURL + '/boletos/'+aNossoNumero+
                                  '?gw-dev-app-key='+Boleto.Cedente.CedenteWS.KeyUser;

    tpConsultaDetalhe : FPURL := FPURL + '/consulta?agencia='+Boleto.Cedente.Agencia
                                + '&posto='+Boleto.Cedente.AgenciaDigito
                                + '&cedente='+Boleto.Cedente.CodigoCedente
                                          + '&nossonumero='+aNossoNumero;
    tpBaixa    : FPURL := FPURL + '/comandoInstrucao';
  end;

end;

procedure TBoletoW_Sicredi_API.DefinirContentType;
begin
 FPContentType := C_CONTENT_TYPE;
end;


procedure TBoletoW_Sicredi_API.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_Sicredi_API.GerarDados;
begin
  if Assigned(Boleto) then
   case Boleto.Configuracoes.WebService.Operacao of
     tpInclui:
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Incluir
         RequisicaoJson;
       end;
     tpAltera:
       begin
         FMetodoHTTP:= htPost;  // Define Método PATCH para alteracao
         RequisicaoAltera;
       end;
     tpBaixa :
       begin
         FMetodoHTTP:= htPost;  // Define Método POST para Baixa
         RequisicaoBaixa;
       end;
     tpConsulta :
       begin
         FMetodoHTTP:= htGET;   //Define Método GET Consulta
         RequisicaoConsulta;
       end;
     tpConsultaDetalhe :
       begin
         FMetodoHTTP:= htGET;   //Define Método GET Consulta Detalhe
         RequisicaoConsultaDetalhe;
       end;

   else
     raise EACBrBoletoWSException.Create(ClassName + Format(
       S_OPERACAO_NAO_IMPLEMENTADO, [
       TipoOperacaoToStr(
       Boleto.Configuracoes.WebService.Operacao)]));
   end;

end;

procedure TBoletoW_Sicredi_API.DefinirAuthorization;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Sicredi_API.GerarTokenAutenticacao: string;
var
  Data  : string;
  Json  : TJSONObject;
  Stream: TMemoryStream;
begin
  Json := TJsonObject.Create; 
  Stream:= TMemoryStream.Create;
  try
      if( not Assigned( FDFeSSL ) ) then
        FDFeSSL := TDFeSSL( Boleto.Configuracoes.WebService);

      FDFeSSL.SSLHttpClass.MimeType := FPContentType;   

      with FDFeSSL.SSLHttpClass.HeaderReq do
        begin
          AddHeader('token', Boleto.Cedente.CedenteWS.ClientID);
        end;

      FDFeSSL.SSLHttpClass.DataReq.LoadFromStream(Stream);
      FDFeSSL.HTTPMethod(MetodoHTTPToStr(htPOST), C_URL_OAUTH_PROD );
      data :=  UTF8Decode(ReadStrFromStream(FDFeSSL.SSLHttpClass.DataResp, FDFeSSL.SSLHttpClass.DataResp.Size ));
      Json.Parse( data );

      if( Json.Values['codigo' ].asString = '' ) then // se não veio código de erro
        begin
          result := Json.Values['chaveTransacao'].asString;
        end
      else
        begin
          raise EACBrBoletoWSException.Create(ClassName
                                                  + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO,
                                                            [ 'Código: '
                                                              + '-'
                                                              +Json.Values['codigo'].asString
                                                              +#13
                                                              +Json.Values['mensagem'].asString
                                                              +#13
                                                              +'Parametro: '
                                                              +Json.Values['parametro'].asString ] ));
        end;
  finally
    Stream.Free;
	Json.Free;
    FDFeSSL.SSLHttpClass.HeaderReq.Clear;
    FDFeSSL.SSLHttpClass.HeaderResp.Clear;
    FDFeSSL.SSLHttpClass.Clear;
  end;
end;

procedure TBoletoW_Sicredi_API.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Sicredi_API.DefinirParametros: String;
var
  Consulta : TStringList;
  Documento : String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin

      if (Boleto.Cedente.Agencia = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o agenciaBeneficiario. ');

      if (Boleto.Cedente.Conta = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o contaBeneficiario. ');

      Documento := OnlyNumber(Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

      Consulta := TStringList.Create;
      Consulta.Delimiter := '&';
      try

        case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
          isbBaixado:
            begin
              Consulta.Add( 'tipoData=DATA_LIQUIDACAO' );
              Consulta.Add( 'dataInicio=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio, 'DD/MM/YYYY'));
              Consulta.Add( 'dataFim=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal, 'DD/MM/YYYY'));
            end;
          isbAberto:
            begin

              if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
              begin
                Consulta.Add( 'tipoData=DATA_VENCIMENTO' );
                Consulta.Add( 'dataInicio=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio, 'DD/MM/YYYY'));
                Consulta.Add( 'dataFim=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal, 'DD/MM/YYYY'));
              end;

              if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0 then
              begin
                Consulta.Add( 'tipoData=DATA_EMISSAO' );
                Consulta.Add( 'dataInicio=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio, 'DD/MM/YYYY'));
                Consulta.Add( 'dataFim=' + FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal, 'DD/MM/YYYY'));
              end;
            end;
        end;

      finally
        Result := Consulta.DelimitedText;
        Consulta.Free;
      end;

  end;

end;

procedure TBoletoW_Sicredi_API.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Sicredi_API.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'),2);
end;

procedure TBoletoW_Sicredi_API.RequisicaoJson;
var
  Data: string;
  Json: TJSONObject;
  aSeuNumero, aNossoNumero:String;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try

      aNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));;
      aSeuNumero    := ATitulo.NossoNumero;// ATitulo.SeuNumero;
      Json.Add('agencia' ).Value.asString                              := Boleto.Cedente.Agencia;
      Json.Add('posto' ).Value.asString                                := Boleto.Cedente.AgenciaDigito;
      Json.Add('cedente' ).Value.asString                              := Boleto.Cedente.CodigoCedente;
      Json.Add('especieDocumento').Value.AsString                      := codigoTipoTitulo(ATitulo.EspecieDoc);
      Json.Add('dataVencimento').Value.AsString                        := FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY');
      Json.Add('valor').Value.AsNumber                                 := ATitulo.ValorDocumento;
      Json.Add('mensagem').Value.AsString                              := UpperCase(Copy(Trim(ATitulo.Instrucao1 +' '+ATitulo.Instrucao2+' '+ATitulo.Instrucao3),0,165));
      Json.Add('descontoAntecipado').Value.AsNumber                    := ATitulo.ValorAbatimento;


      GerarDesconto(Json);
      GerarJuros(Json);
      GerarMulta(Json);
      GerarPagador(Json);
      GerarBenificiarioFinal(Json);
      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        Json.Add('numDiasNegativacao').Value.AsInteger                  := ATitulo.DiasDeNegativacao;
      end;

      Json.Values['seuNumero' ].asString                            := aSeuNumero;
      Json.Values['nossoNumero' ].asString                          := aNossoNumero;

      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_Sicredi_API.RequisicaoAltera;
var
  Data: string;
  Json: TJSONObject;
begin

  if Assigned(ATitulo) then
  begin

    Json := TJsonObject.Create;

    Json.Add( 'agencia' ).Value.asString                              := Boleto.Cedente.Agencia;
    Json.Add( 'posto' ).Value.asString                                := Boleto.Cedente.AgenciaDigito;
    Json.Add( 'cedente' ).Value.asString                              := Boleto.Cedente.CodigoCedente;
    Json.Add( 'nossonumero' ).Value.asString                          := ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo);

    try

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3:  // RemessaConcederAbatimento
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'CONCESSAO_ABATIMENTO';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            AtribuirAbatimento(Json);
          end;
        4:  // RemessaCancelarAbatimento
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'CANCELAMENTO_ABATIMENTO_CONCEDIDO';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            AlteracaoAbatimento(Json);
          end;
        5: //RemessaConcederDesconto
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_OUTROS_DADOS';
            Json.Add( 'complementoInstrucao' ).Value.asString := 'DESCONTO';
            AtribuirDesconto(Json);
          end;
        7: //RemessaAlterarVencimento
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_VENCIMENTO';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            Json.Add('tipoVencimento' ).Value.asString         :=  'DATA_ESPECIFICA';
            AlteraDataVencimento(Json);
          end;
        9:  //RemessaProtestar
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'PEDIDO_PROTESTO';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            AlterarProtesto(Json);
          end;
        10:  //RemessaSustarProtesto
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'SUSTAR_PROTESTO_MANTER_CARTEIRA';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            AlterarProtesto(Json);
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'SUSTAR_PROTESTO_BAIXAR_TITULO';
            Json.Add('complementoInstrucao' ).IsJsonNull( 'null' );
            AlterarProtesto(Json);
          end;
        13:  //RemessaDispensarJuros
          begin
            //Json.Add('indicadorDispensarJuros').Value.AsString := 'S';
          end;
        14:  //RemessaAlterarNomeEnderecoSacado
          begin
            //Json.Add('indicadorAlterarEnderecoPagador').Value.AsString := 'S';
            AlteracaoEnderecoPagador(Json);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_SEU_NUMERO';
            Json.Add('complementoInstrucao' ).IsJsonNull('null');
            AlteracaoSeuNumero(Json);
          end;
        37: //RemessaCobrarJurosMora
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_OUTROS_DADOS';
            Json.Add('complementoInstrucao' ).Value.asString   := 'JUROS_DIA';
            AtribuirJuros(Json);
          end;
        50:  //RemessaAlterarMulta
          begin
            AtribuirMulta(Json);
          end;
        51:  //RemessaDispensarMulta
          begin
            //Json.Add('indicadorDispensarMulta').Value.AsString := 'S';
          end;
        52: //RemessaAlterarDesconto
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_OUTROS_DADOS';
            Json.Add('complementoInstrucao' ).Value.asString   := 'DESCONTO';
            AlteracaoDesconto(Json);
          end;
        53: //RemessaAlterarDataDesconto
          begin
            Json.Add( 'instrucaoComando' ).Value.asString := 'ALTERACAO_OUTROS_DADOS';
            Json.Add('complementoInstrucao' ).Value.asString   := 'DATA_LIMITE_CONCESSAO_DESCONTO';
            AlteracaoDataDesconto(Json);
          end;
        55:  //RemessaAlterarPrazoLimiteRecebimento
          begin
            AlteracaoPrazo(Json);
          end;
        66:  //RemessaNegativacaoSemProtesto
          begin
            Json.Add('indicadorNegativar').Value.AsString := 'S';
            AtribuirNegativacao(Json);
          end;
      end;

      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;

  end;

end;

procedure TBoletoW_Sicredi_API.GeraDadosInstrucao(AJson: TJsonObject);
var
aNossoNumero:String;
begin
    aNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));
    AJson.Add('agencia' ).Value.asString           := Boleto.Cedente.Agencia;
    AJson.Add('posto' ).Value.asString             := Boleto.Cedente.AgenciaDigito;
    AJson.Add('cedente' ).Value.asString           := Boleto.Cedente.CodigoCedente;
    AJson.Add('nossoNumero' ).Value.asString       := aNossoNumero;
end;


procedure TBoletoW_Sicredi_API.RequisicaoBaixa;
var
  Data: string;
  AJson: TJSONObject;
begin


  if Assigned(ATitulo) then
  begin

    AJson := TJSONObject.Create;

    if Assigned(AJson) then
    begin
      GeraDadosInstrucao(AJson );
      AJson.Add( 'instrucaoComando' ).Value.asString := 'PEDIDO_BAIXA';
      AJson.Add('complementoInstrucao' ).IsJsonNull('null');
    end;

    Data := AJson.Stringify;
    FPDadosMsg := Data;

  end;

end;

procedure TBoletoW_Sicredi_API.RequisicaoConsulta;
begin
   //Sem Payload - Define Método GET
end;

procedure TBoletoW_Sicredi_API.RequisicaoConsultaDetalhe;
begin
    //Sem Payload - Define Método GET
end;

procedure TBoletoW_Sicredi_API.GerarPagador(AJson: TJsonObject);

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
        aJson.Add('tipoPessoa').Value.AsInteger      := StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2'));
        aJson.Add('cpfCnpj').Value.asString          := OnlyNumber(ATitulo.Sacado.CNPJCPF);
        aJson.Add('nome').Value.AsString             := ATitulo.Sacado.NomeSacado;
        aJson.Add('endereco').Value.AsString         := ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero;
        aJson.Add('cep').Value.AsString              := OnlyNumber(ATitulo.Sacado.CEP);
        aJson.Add('cidade').Value.AsString           := ATitulo.Sacado.Cidade;
        aJson.Add('uf').Value.AsString               := ATitulo.Sacado.UF;
        aJson.Add('telefone').Value.AsString         := IfThen( ATitulo.Sacado.Fone='' , '0',ATitulo.Sacado.Fone );
        aJson.Add('email').Value.AsString            := ATitulo.Sacado.Email;
    end;

  end;

end;

procedure TBoletoW_Sicredi_API.GerarBenificiarioFinal(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
      if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        // No manual não constam dados para informar Avalista
        AJson.Add( 'codigoSacadorAvalista' ).Value.asString := '000';
      end;
  end;
end;

procedure TBoletoW_Sicredi_API.GerarJuros(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin

    if Assigned(AJson) then
    begin
      AJson.Add( 'tipoJuros' ).Value.asString  := ATitulo.CodigoMora;
      AJson.Add( 'juros' ).Value.AsNumber      := ATitulo.ValorMoraJuros;
    end;

  end;
end;

procedure TBoletoW_Sicredi_API.GerarMulta(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin

      if ATitulo.PercentualMulta > 0 then
      begin
        AJson.Add( 'multas' ).Value.asNumber := ATitulo.PercentualMulta;
      end;
    end;
  end;
end;

procedure TBoletoW_Sicredi_API.GerarDesconto(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin

    if Assigned(AJson) then
    begin
        AJson.Add('tipoDesconto').Value.AsString         := ifThen( integer(ATitulo.TipoDesconto)=1,'A' , 'B' );
        if (ATitulo.DataDesconto > 0) then
        begin
          AJson.Add('valorDesconto1').Value.AsNumber       := ATitulo.ValorDesconto;
          AJson.Add('dataDesconto1').Value.AsString        := FormatDateBr(ATitulo.DataDesconto, 'DD/MM/YYYY');
        end;

        if (ATitulo.DataDesconto2 > 0) then
        begin
          AJson.Add('valorDesconto2').Value.AsNumber       := ATitulo.ValorDesconto2;
          AJson.Add('dataDesconto2').Value.AsString        := FormatDateBr(ATitulo.DataDesconto2, 'DD/MM/YYYY');
        end;

        if (ATitulo.DataDesconto3 > 0) then
        begin
          AJson.Add('valorDesconto3').Value.AsNumber       := ATitulo.ValorDesconto3;
          AJson.Add('dataDesconto3').Value.AsString        := FormatDateBr(ATitulo.DataDesconto3, 'DD/MM/YYYY');
        end;

    end;

  end;

end;

procedure TBoletoW_Sicredi_API.AlteraDataVencimento(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('data1' ).Value.asString                  := FormatDateBr(ATitulo.Vencimento, 'DD.MM.YYYY');
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AtribuirDesconto(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin

    if Assigned(AJson) then
    begin
      AJson.Add('valor1' ).Value.asNumber       := ATitulo.ValorDesconto;
    end;

  end;

end;

procedure TBoletoW_Sicredi_API.AlteracaoDesconto(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('valor1' ).Value.asNumber       := ATitulo.ValorDesconto;
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AlteracaoDataDesconto(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('data1' ).Value.asString                  := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
    end;
  end;

end;


procedure TBoletoW_Sicredi_API.AlterarProtesto(AJson: TJsonObject);
begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('complementoInstrucao' ).IsJsonNull('null');
    end;
  end;
end;

procedure TBoletoW_Sicredi_API.AtribuirAbatimento(AJson: TJsonObject);

begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('valor1' ).Value.asNumber := ATitulo.ValorAbatimento;
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AlteracaoAbatimento(AJson: TJsonObject);

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('complementoInstrucao' ).IsJsonNull( 'null' );
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AtribuirJuros(AJson: TJsonObject);

begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('valor1' ).Value.asNumber := ATitulo.ValorMoraJuros;
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AtribuirMulta(AJson: TJsonObject);
begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_API.AtribuirNegativacao(AJson: TJsonObject);
begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_API.AlteracaoSeuNumero(AJson: TJsonObject);

begin

  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.Add('seuNumero' ).Value.asString := ATitulo.SeuNumero;
    end;
  end;

end;

procedure TBoletoW_Sicredi_API.AlteracaoEnderecoPagador(AJson: TJsonObject);

begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_API.AlteracaoPrazo(AJson: TJsonObject);

begin
// sem Payload
end;

function TBoletoW_Sicredi_API.CodigoTipoTitulo(AEspecieDoc : String): String;
begin

  result := 'A';
  { Pegando o tipo de AEspecieDoc }
  if AEspecieDoc = 'DMI' then
     result   := 'A'
  else if AEspecieDoc = 'DR' then
     result   := 'B'
  else if AEspecieDoc = 'NP' then
     result   := 'C'
  else if AEspecieDoc = 'NR' then
     result   := 'D'
  else if AEspecieDoc = 'NS' then
     result   := 'E'
  else if AEspecieDoc = 'RC' then
     result   := 'G'
  else if AEspecieDoc = 'LC' then
     result   := 'H'
  else if AEspecieDoc = 'ND' then
     result   := 'I'
  else if AEspecieDoc = 'DSI' then
     result   := 'J'
  else if AEspecieDoc = 'OS' then
     result   := 'K'
  else
     result := 'A';

end;

constructor TBoletoW_Sicredi_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := OAuth.Ambiente = taHomologacao;
  end;

end;

function TBoletoW_Sicredi_API.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;

end;

function TBoletoW_Sicredi_API.Enviar: boolean;
begin
  Result := inherited Enviar;

end;
end.
