{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales          }

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

unit ACBrBoletoW_BancoBrasil_API;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  ACBrBoleto,
  ACBrBoletoWS.Rest,
  Jsons, ACBrUtil.Strings;


type

  { TBoletoW_BancoBrasil_API }
  TBoletoW_BancoBrasil_API = class(TBoletoWSREST)
  private
    function CodigoTipoTitulo(AEspecieDoc:String): Integer;
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
    procedure RequisicaoPIXCriar;
    procedure RequisicaoPIXCancelar;
    procedure RequisicaoPIXConsultar;
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarBenificiarioFinal(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);

    procedure AlteraDataVencimento(AJson: TJsonObject);
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
  C_URL            = 'https://api.bb.com.br/cobrancas/v2';
  C_URL_HOM        = 'https://api.hm.bb.com.br/cobrancas/v2';

  C_URL_OAUTH_PROD = 'https://oauth.bb.com.br/oauth/token';
  C_URL_OAUTH_HOM  = 'https://oauth.sandbox.bb.com.br/oauth/token';

  C_ACCEPT         = 'application/json';
  C_AUTHORIZATION  = 'Authorization';

const CHARS_VALIDOS : TSetOfChars = ['A'..'Z','0'..'9',' ',',','-','.',
                                     'À','Á','Â','Ã','Ä','Å','È','É','Ê',
                                     'Ë','Ì','Í','Î','Ï','Ò','Ó','Ô','Õ',
                                     'Ö','Ù','Ú','Û','Ü'];
implementation

uses
  synacode, strutils, DateUtils, ACBrUtil.DateTime;

{ TBoletoW_BancoBrasil_API }

procedure TBoletoW_BancoBrasil_API.DefinirURL;
var DevAPP, ID, NConvenio : String;
begin
  FPURL     := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,C_URL, C_URL_HOM);
  DevAPP    := '?gw-dev-app-key='+Boleto.Cedente.CedenteWS.KeyUser;

  if ATitulo <> nil then
    ID      := ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo);

  NConvenio := OnlyNumber(Boleto.Cedente.Convenio);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui           : FPURL := FPURL + '/boletos' + DevAPP;
    tpConsulta         : FPURL := FPURL + '/boletos' + DevAPP + '&' + DefinirParametros;
    tpAltera           : FPURL := FPURL + '/boletos/'+ ID + DevAPP;
    tpConsultaDetalhe  : FPURL := FPURL + '/boletos/'+ ID + DevAPP + '&numeroConvenio='+ NConvenio;
    tpBaixa            : FPURL := FPURL + '/boletos/'+ ID + '/baixar'+DevAPP;
    tpPIXCriar         : FPURL := FPURL + '/boletos/'+ ID + '/gerar-pix' + DevAPP;
    tpPIXCancelar      : FPURL := FPURL + '/boletos/'+ ID + '/cancelar-pix' + DevAPP;
    tpPIXConsultar     : FPURL := FPURL + '/boletos/'+ ID + '/pix' + DevAPP + '&numeroConvenio='+ NConvenio;
  end;

end;

procedure TBoletoW_BancoBrasil_API.DefinirContentType;
begin
  FPContentType := 'application/json';
end;


procedure TBoletoW_BancoBrasil_API.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_BancoBrasil_API.GerarDados;
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
         FMetodoHTTP:= htPATCH;  // Define Método PATCH para alteracao
         RequisicaoAltera;
       end;
     tpBaixa :
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Baixa
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
     tpPIXCriar :
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Criar o PIX
         RequisicaoPIXCriar;
       end;
     tpPIXCancelar :
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Cancelar o PIX
         RequisicaoPIXCancelar;
       end;
     tpPIXConsultar :
       begin
         FMetodoHTTP:= htGET;   //Define Método GET Consulta PIX
         RequisicaoPIXConsultar;
       end

   else
     raise EACBrBoletoWSException.Create(ClassName + Format(
       S_OPERACAO_NAO_IMPLEMENTADO, [
       TipoOperacaoToStr(
       Boleto.Configuracoes.WebService.Operacao)]));
   end;

end;

procedure TBoletoW_BancoBrasil_API.DefinirAuthorization;
begin
  FPAuthorization := C_Authorization + ': ' + 'Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_BancoBrasil_API.GerarTokenAutenticacao: string;
begin
  Result := inherited GerarTokenAutenticacao;

  {result:= '';
  if Assigned(OAuth) then
  begin
    if OAuth.GerarToken then
      result := OAuth.Token
    else
      raise EACBrBoletoWSException.Create(ClassName + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO, [OAuth.ErroComunicacao] ));
  end;}

end;

procedure TBoletoW_BancoBrasil_API.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';

end;

function TBoletoW_BancoBrasil_API.DefinirParametros: String;
var
  Consulta : TStringList;
  Documento : String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
      if Boleto.Configuracoes.WebService.Filtro.indicadorSituacao = isbNenhum then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o indicadorSituacao diferente de isbNenhum. ');

      if (Boleto.Cedente.Agencia = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o agenciaBeneficiario. ');

      if (Boleto.Cedente.Conta = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o contaBeneficiario. ');

      Documento := OnlyNumber(Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

      Consulta := TStringList.Create;
      try
        Consulta.Delimiter := '&';
        Consulta.Add('indicadorSituacao='+IfThen(Boleto.Configuracoes.WebService.Filtro.indicadorSituacao = isbBaixado,'B','A'));

        if Boleto.Configuracoes.WebService.Filtro.contaCaucao > 0 then
          Consulta.Add('contaCaucao='+ IntToStr(Boleto.Configuracoes.WebService.Filtro.contaCaucao));

        Consulta.Add('agenciaBeneficiario='+OnlyNumber( Boleto.Cedente.Agencia ));
        Consulta.Add('contaBeneficiario='+OnlyNumber( Boleto.Cedente.Conta ));

        if Boleto.Configuracoes.WebService.Filtro.carteira > 0 then
          Consulta.Add('carteiraConvenio='+IntToStr(Boleto.Configuracoes.WebService.Filtro.carteira));

        if Boleto.Configuracoes.WebService.Filtro.carteiraVariacao > 0 then
          Consulta.Add('variacaoCarteiraConvenio='+IntToStr(Boleto.Configuracoes.WebService.Filtro.carteiraVariacao));

        if Boleto.Configuracoes.WebService.Filtro.modalidadeCobranca > 0 then
          Consulta.Add('modalidadeCobranca='+ IntToStr(Boleto.Configuracoes.WebService.Filtro.modalidadeCobranca));

        if Length(Documento) = 14 then
        begin
          Consulta.Add('cnpjPagador='+Copy(Documento,1,12));
          Consulta.Add('digitoCNPJPagador='+Copy(Documento,13,2));
        end else
        if Length(Documento) = 11 then
        begin
          Consulta.Add('cpfPagador='+Copy(Documento,1,9));
          Consulta.Add('digitoCPFPagador='+Copy(Documento,10,2));
        end;

        if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
          Consulta.Add('dataInicioVencimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal > 0 then
          Consulta.Add('dataFimVencimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0 then
          Consulta.Add('dataInicioRegistro='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal > 0 then
          Consulta.Add('dataFimRegistro='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio > 0 then
          Consulta.Add('dataInicioMovimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal > 0 then
          Consulta.Add('dataFimMovimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.codigoEstadoTituloCobranca > 0 then
          Consulta.Add('codigoEstadoTituloCobranca='+intToStr(Boleto.Configuracoes.WebService.Filtro.codigoEstadoTituloCobranca));

        if not (Boleto.Configuracoes.WebService.Filtro.boletoVencido = ibvNenhum) then
          Consulta.Add('boletoVencido='+IfThen(Boleto.Configuracoes.WebService.Filtro.boletoVencido = ibvSim,'S','N'));

        if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
          Consulta.Add('indice='+ FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade));

      finally
        Result := Consulta.DelimitedText;
        Consulta.Free;
      end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_BancoBrasil_API.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'),2);
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoJson;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('numeroConvenio').Value.AsNumber                         := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);
      Json.Add('numeroCarteira').Value.AsInteger                        := StrToIntDef(OnlyNumber(ATitulo.Carteira),0);
      Json.Add('numeroVariacaoCarteira').Value.AsInteger                := StrToIntDef(OnlyNumber(Boleto.Cedente.Modalidade),0);
      Json.Add('codigoModalidade').Value.AsInteger                      := 1;

      Json.Add('dataEmissao').Value.AsString                            := FormatDateBr(ATitulo.DataDocumento, 'DD.MM.YYYY');
      Json.Add('dataVencimento').Value.AsString                         := FormatDateBr(ATitulo.Vencimento, 'DD.MM.YYYY');
      Json.Add('valorOriginal').Value.AsNumber                          := ATitulo.ValorDocumento;
      Json.Add('valorAbatimento').Value.AsNumber                        := ATitulo.ValorAbatimento;
      if (ATitulo.DataProtesto > 0) then
        Json.Add('quantidadeDiasProtesto').Value.AsInteger              := Trunc(ATitulo.DataProtesto - ATitulo.Vencimento);
      if (ATitulo.DataLimitePagto > 0 ) then
      begin
        Json.Add('indicadorAceiteTituloVencido').Value.AsString         := 'S';
        Json.Add('numeroDiasLimiteRecebimento').Value.AsInteger         := Trunc(ATitulo.DataLimitePagto - ATitulo.Vencimento);
      end;
      Json.Add('codigoAceite').Value.AsString                           := IfThen(ATitulo.Aceite = atSim,'A','N');
      Json.Add('codigoTipoTitulo').Value.AsInteger                      := codigoTipoTitulo(ATitulo.EspecieDoc);
      Json.Add('descricaoTipoTitulo').Value.AsString                    := ATitulo.EspecieDoc;

      if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
        Json.Add('indicadorPermissaoRecebimentoParcial').Value.AsString := 'S';

      Json.Add('numeroTituloBeneficiario').Value.AsString               := Copy(Trim(UpperCase(ATitulo.NumeroDocumento)),0,15);

      Json.Add('campoUtilizacaoBeneficiario').Value.AsString            := Copy(Trim(OnlyCharsInSet(AnsiUpperCase(ATitulo.Mensagem.Text),CHARS_VALIDOS)),0,30);
      Json.Add('numeroTituloCliente').Value.AsString                    := Boleto.Banco.MontarCampoNossoNumero(ATitulo);
      Json.Add('mensagemBloquetoOcorrencia').Value.AsString             := UpperCase(Copy(Trim(ATitulo.Instrucao1 +' '+ATitulo.Instrucao2+' '+ATitulo.Instrucao3),0,165));
      GerarDesconto(Json);
      GerarJuros(Json);
      GerarMulta(Json);
      GerarPagador(Json);
      GerarBenificiarioFinal(Json);
      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        Json.Add('quantidadeDiasNegativacao').Value.AsInteger           := ATitulo.DiasDeNegativacao;
        Json.Add('orgaoNegativador').Value.AsInteger                    := StrToInt64Def(ATitulo.orgaoNegativador,0);
      end;
      Json.Add('indicadorPix').Value.AsString := IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'S','N');

      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXCancelar;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('numeroConvenio').Value.AsNumber := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);
      Data := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXConsultar;
begin
  //Sem Payload - Define Método GET
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXCriar;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('numeroConvenio').Value.AsNumber := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);
      Data := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoAltera;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin

    Json := TJsonObject.Create;
    try
      Json.Add('numeroConvenio').Value.AsNumber := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3:  // RemessaConcederAbatimento
          begin                                 
            Json.Add('indicadorIncluirAbatimento').Value.AsString := 'S';
            AtribuirAbatimento(Json);
          end;
        4:  // RemessaCancelarAbatimento
          begin                                 
            Json.Add('indicadorAlterarAbatimento').Value.AsString := 'S';
            AlteracaoAbatimento(Json);
          end;
        5: //RemessaConcederDesconto
          begin
            Json.Add('indicadorAtribuirDesconto').Value.AsString := 'S';
            AtribuirDesconto(Json);
          end;
        7: //RemessaAlterarVencimento
          begin
            Json.Add('indicadorNovaDataVencimento').Value.AsString := 'S';
            AlteraDataVencimento(Json);
          end;
        9:  //RemessaProtestar
          begin
            Json.Add('indicadorProtestar').Value.AsString := 'S';
            AlterarProtesto(Json);
          end;
        10:  //RemessaSustarProtesto
          begin
            Json.Add('indicadorSustacaoProtesto').Value.AsString := 'S';
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            Json.Add('indicadorCancelarProtesto').Value.AsString := 'S';
          end;
        13:  //RemessaDispensarJuros
          begin
            Json.Add('indicadorDispensarJuros').Value.AsString := 'S';
          end;
        14:  //RemessaAlterarNomeEnderecoSacado
          begin
            Json.Add('indicadorAlterarEnderecoPagador').Value.AsString := 'S';
            AlteracaoEnderecoPagador(Json);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            Json.Add('indicadorAlterarSeuNumero').Value.AsString := 'S';
            AlteracaoSeuNumero(Json);
          end;
        37: //RemessaCobrarJurosMora
          begin
            Json.Add('indicadorCobrarJuros').Value.AsString := 'S';
            AtribuirJuros(Json);
          end;
        50:  //RemessaAlterarMulta
          begin
            Json.Add('indicadorCobrarMulta').Value.AsString := 'S';
            AtribuirMulta(Json);
          end;
        51:  //RemessaDispensarMulta
          begin
            Json.Add('indicadorDispensarMulta').Value.AsString := 'S';
          end;
        52: //RemessaAlterarDesconto
          begin
            Json.Add('indicadorAlterarDesconto').Value.AsString := 'S';
            AlteracaoDesconto(Json);
          end;
        53: //RemessaAlterarDataDesconto
          begin
            Json.Add('indicadorAlterarDataDesconto').Value.AsString := 'S';
            AlteracaoDataDesconto(Json);
          end;
        55:  //RemessaAlterarPrazoLimiteRecebimento
          begin
            Json.Add('indicadorAlterarPrazoBoletoVencido').Value.AsString := 'S';
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

procedure TBoletoW_BancoBrasil_API.RequisicaoBaixa;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('numeroConvenio').Value.AsNumber := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);
      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;

end;

procedure TBoletoW_BancoBrasil_API.RequisicaoConsulta;
begin
   //Sem Payload - Define Método GET
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoConsultaDetalhe;
begin
    //Sem Payload - Define Método GET
end;

procedure TBoletoW_BancoBrasil_API.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDadosPagador := TJSONObject.Create;

      try
        JsonDadosPagador.Add('tipoInscricao').Value.AsInteger   := StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2'));
        JsonDadosPagador.Add('numeroInscricao').Value.AsNumber  := StrToInt64(OnlyNumber(ATitulo.Sacado.CNPJCPF));
        JsonDadosPagador.Add('nome').Value.AsString             := ATitulo.Sacado.NomeSacado;
        JsonDadosPagador.Add('endereco').Value.AsString         := ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero;
        JsonDadosPagador.Add('cep').Value.AsInteger             := StrToInt(OnlyNumber(ATitulo.Sacado.CEP));
        JsonDadosPagador.Add('cidade').Value.AsString           := ATitulo.Sacado.Cidade;
        JsonDadosPagador.Add('bairro').Value.AsString           := ATitulo.Sacado.Bairro;
        JsonDadosPagador.Add('uf').Value.AsString               := ATitulo.Sacado.UF;
        //JsonDadosPagador.Add('telefone').Value.AsString         :=

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

procedure TBoletoW_BancoBrasil_API.GerarBenificiarioFinal(AJson: TJsonObject);
var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
      if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        JsonSacadorAvalista := TJSONObject.Create;

        try
          JsonSacadorAvalista.Add('tipoInscricao').Value.AsInteger   :=  StrToInt(IfThen( Length( OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,'1','2'));
          JsonSacadorAvalista.Add('numeroInscricao').Value.AsNumber  :=  StrToInt64Def(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF),0);
          JsonSacadorAvalista.Add('nome').Value.AsString             :=  ATitulo.Sacado.SacadoAvalista.NomeAvalista;

          JsonPairSacadorAvalista := TJsonPair.Create(AJson, 'beneficiarioFinal');
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

procedure TBoletoW_BancoBrasil_API.GerarJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonJuros := TJSONObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          if ATitulo.CodigoMora = '' then
          begin
            case aTitulo.CodigoMoraJuros of
              cjValorDia: aTitulo.CodigoMora   := '1';
              cjTaxaMensal: aTitulo.CodigoMora := '2';
              cjIsento: aTitulo.CodigoMora     := '3';
            end;
          end;

          JsonJuros.Add('tipo').Value.AsInteger             := StrToIntDef(ATitulo.CodigoMora, 3);
          case (StrToIntDef(ATitulo.CodigoMora, 3)) of
            1 : JsonJuros.Add('valor').Value.AsNumber       := ATitulo.ValorMoraJuros;
            2 : JsonJuros.Add('porcentagem').Value.AsNumber := ATitulo.ValorMoraJuros;
          end;

          JsonPairJuros := TJsonPair.Create(AJson, 'jurosMora');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('jurosMora').Assign(JsonPairJuros);
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

procedure TBoletoW_BancoBrasil_API.GerarMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: integer;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJSONObject.Create;

      try
        if ATitulo.PercentualMulta > 0 then
        begin
          if ATitulo.MultaValorFixo then
            ACodMulta := 1
          else
            ACodMulta := 2;
        end
        else
          ACodMulta := 0;


        if (ATitulo.DataMulta > 0) then
        begin
          JsonMulta.Add('tipo').Value.AsInteger             := ACodMulta;

          if( aCodMulta > 0 ) then
            JsonMulta.Add('data').Value.AsString              := FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY');

          case ACodMulta of
            1 : JsonMulta.Add('valor').Value.AsNumber       := ATitulo.PercentualMulta;
            2 : JsonMulta.Add('porcentagem').Value.AsNumber := ATitulo.PercentualMulta;
          end;

          JsonPairMulta := TJsonPair.Create(AJson, 'multa');
          try
            JsonPairMulta.Value.AsObject := JsonMulta;
            AJson.Add('multa').Assign(JsonPairMulta);
          finally
            JsonPairMulta.Free;
          end;
        end;
      finally
        JsonMulta.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.GerarDesconto(AJson: TJsonObject);
var
  JsonDesconto: TJsonObject;
  JsonPairDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDesconto := TJSONObject.Create;
      try

        if (ATitulo.DataDesconto > 0) then
        begin
          JsonDesconto.Add('tipo').Value.AsInteger             := integer(ATitulo.TipoDesconto);
          JsonDesconto.Add('dataExpiracao').Value.AsString     := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
          case integer(ATitulo.TipoDesconto) of
            1 : JsonDesconto.Add('valor').Value.AsNumber       := ATitulo.ValorDesconto;
            2 : JsonDesconto.Add('porcentagem').Value.AsNumber := ATitulo.ValorDesconto;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'desconto');
          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('desconto').Assign(JsonPairDesconto);
          finally
            JsonPairDesconto.Free;
          end;
        end;
      finally
        JsonDesconto.Free;
      end;

      JsonDesconto := TJSONObject.Create;
      try
        JsonDesconto.Add('tipo').Value.AsInteger := integer(ATitulo.TipoDesconto);
        if ATitulo.DataDesconto2 > 0 then
        begin
          JsonDesconto.Add('dataExpiracao').Value.AsString     := FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY');
          case integer(ATitulo.TipoDesconto2) of
            1 : JsonDesconto.Add('valor').Value.AsNumber       := ATitulo.ValorDesconto2;
            2 : JsonDesconto.Add('porcentagem').Value.AsNumber := ATitulo.ValorDesconto2;
          end;

          JsonPairDesconto                  := TJsonPair.Create(AJson, 'segundoDesconto');
          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('segundoDesconto').Assign(JsonPairDesconto);
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

procedure TBoletoW_BancoBrasil_API.AlteraDataVencimento(AJson: TJsonObject);
var
  JsonVencimento: TJsonObject;
  JsonPairVencimento: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonVencimento := TJSONObject.Create;
      try
        if (ATitulo.Vencimento > 0) then
        begin
          JsonVencimento.Add('novaDataVencimento').Value.AsString := FormatDateBr(ATitulo.Vencimento, 'DD.MM.YYYY');

          JsonPairVencimento := TJsonPair.Create(AJson, 'alteracaoData');
          try
            JsonPairVencimento.Value.AsObject := JsonVencimento;
            AJson.Add('alteracaoData').Assign(JsonPairVencimento);
          finally
            JsonPairVencimento.Free;
          end;
        end;
      finally
        JsonVencimento.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AtribuirDesconto(AJson: TJsonObject);
var
  JsonAtribuirDesconto: TJsonObject;
  JsonPairAtribuirDesconto: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirDesconto := TJSONObject.Create;
      try
        if (ATitulo.ValorDesconto > 0) then
        begin
          JsonAtribuirDesconto.Add('tipoPrimeiroDesconto').Value.AsInteger := integer(ATitulo.TipoDesconto);
          case integer(ATitulo.TipoDesconto) of
            1 : JsonAtribuirDesconto.Add('valorPrimeiroDesconto').Value.AsNumber := ATitulo.ValorDesconto;
            2 : JsonAtribuirDesconto.Add('percentualPrimeiroDesconto').Value.AsNumber := ATitulo.ValorDesconto;
          end;
          JsonAtribuirDesconto.Add('dataPrimeiroDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
        end;
        if (ATitulo.ValorDesconto2 > 0) then
        begin
          JsonAtribuirDesconto.Add('tipoSegundoDesconto').Value.AsInteger := integer(ATitulo.TipoDesconto2);
          case integer(ATitulo.TipoDesconto2) of
            1 : JsonAtribuirDesconto.Add('valorSegundoDesconto').Value.AsNumber := ATitulo.ValorDesconto2;
            2 : JsonAtribuirDesconto.Add('percentualSegundoDesconto').Value.AsNumber := ATitulo.ValorDesconto2;
          end;
          JsonAtribuirDesconto.Add('dataSegundoDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY');
        end;

        if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
        begin
          JsonPairAtribuirDesconto := TJsonPair.Create(AJson, 'desconto');
          try
            JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
            AJson.Add('desconto').Assign(JsonPairAtribuirDesconto);
          finally
            JsonPairAtribuirDesconto.Free;
          end;

        end;

      finally
        JsonAtribuirDesconto.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoDesconto(AJson: TJsonObject);
var
  JsonAtribuirDesconto: TJsonObject;
  JsonPairAtribuirDesconto: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirDesconto := TJSONObject.Create;
      try
        if (ATitulo.ValorDesconto > 0) then
        begin
          JsonAtribuirDesconto.Add('tipoPrimeiroDesconto').Value.AsInteger := integer(ATitulo.TipoDesconto);
          case integer(ATitulo.TipoDesconto) of
            1 : JsonAtribuirDesconto.Add('novoValorPrimeiroDesconto').Value.AsNumber := ATitulo.ValorDesconto;
            2 : JsonAtribuirDesconto.Add('novoPercentualPrimeiroDesconto').Value.AsNumber := ATitulo.ValorDesconto;
          end;
          JsonAtribuirDesconto.Add('novaDataLimitePrimeiroDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
        end;
        if (ATitulo.ValorDesconto2 > 0) then
        begin
          JsonAtribuirDesconto.Add('tipoSegundoDesconto').Value.AsInteger := integer(ATitulo.TipoDesconto2);
          case integer(ATitulo.TipoDesconto2) of
            1 : JsonAtribuirDesconto.Add('novoValorSegundoDesconto').Value.AsNumber := ATitulo.ValorDesconto2;
            2 : JsonAtribuirDesconto.Add('novoPercentualSegundoDesconto').Value.AsNumber := ATitulo.ValorDesconto2;
          end;
          JsonAtribuirDesconto.Add('novaDataLimiteSegundoDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY');
        end;

        if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
        begin
          JsonPairAtribuirDesconto := TJsonPair.Create(AJson, 'alteracaoDesconto');
          try
            JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
            AJson.Add('alteracaoDesconto').Assign(JsonPairAtribuirDesconto);
          finally
            JsonPairAtribuirDesconto.Free;
          end;

        end;

      finally
        JsonAtribuirDesconto.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoDataDesconto(AJson: TJsonObject);
var
  JsonAtribuirDesconto: TJsonObject;
  JsonPairAtribuirDesconto: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirDesconto := TJSONObject.Create;
      try
        if (ATitulo.DataDesconto > 0) then
        begin
          JsonAtribuirDesconto.Add('novaDataLimitePrimeiroDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
        end;
        if (ATitulo.DataDesconto2 > 0) then
        begin
          JsonAtribuirDesconto.Add('novaDataLimiteSegundoDesconto').Value.AsString := FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY');
        end;

        if (ATitulo.DataDesconto > 0) or (ATitulo.DataDesconto2 > 0) then
        begin
          JsonPairAtribuirDesconto := TJsonPair.Create(AJson, 'alteracaoDataDesconto');
          try
            JsonPairAtribuirDesconto.Value.AsObject := JsonAtribuirDesconto;
            AJson.Add('alteracaoDataDesconto').Assign(JsonPairAtribuirDesconto);
          finally
            JsonPairAtribuirDesconto.Free;
          end;

        end;

      finally
        JsonAtribuirDesconto.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlterarProtesto(AJson: TJsonObject);
var
  JsonAtribuirProtesto: TJsonObject;
  JsonPairAtribuirProtesto: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirProtesto := TJSONObject.Create;
      try
        if (ATitulo.DiasDeProtesto > 0) then
        begin
          JsonAtribuirProtesto.Add('quantidadeDiasProtesto').Value.AsInteger := ATitulo.DiasDeProtesto;

          JsonPairAtribuirProtesto := TJsonPair.Create(AJson, 'protesto');
          try
            JsonPairAtribuirProtesto.Value.AsObject := JsonAtribuirProtesto;
            AJson.Add('protesto').Assign(JsonPairAtribuirProtesto);
          finally
            JsonPairAtribuirProtesto.Free;
          end;

        end;

      finally
        JsonAtribuirProtesto.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AtribuirAbatimento(AJson: TJsonObject);
var
  JsonAtribuirAbatimento: TJsonObject;
  JsonPairAtribuirAbatimento: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirAbatimento := TJSONObject.Create;
      try
        if (ATitulo.ValorAbatimento > 0) then
        begin
          JsonAtribuirAbatimento.Add('valorAbatimento').Value.AsNumber := ATitulo.ValorAbatimento;

          JsonPairAtribuirAbatimento := TJsonPair.Create(AJson, 'abatimento');
          try
            JsonPairAtribuirAbatimento.Value.AsObject := JsonAtribuirAbatimento;
            AJson.Add('abatimento').Assign(JsonPairAtribuirAbatimento);
          finally
            JsonPairAtribuirAbatimento.Free;
          end;

        end;

      finally
        JsonAtribuirAbatimento.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoAbatimento(AJson: TJsonObject);
var
  JsonAtribuirAbatimento: TJsonObject;
  JsonPairAtribuirAbatimento: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirAbatimento := TJSONObject.Create;
      try
        if (ATitulo.ValorAbatimento > 0) then
        begin
          JsonAtribuirAbatimento.Add('novoValorAbatimento').Value.AsNumber := ATitulo.ValorAbatimento;

          JsonPairAtribuirAbatimento := TJsonPair.Create(AJson, 'alteracaoAbatimento');
          try
            JsonPairAtribuirAbatimento.Value.AsObject := JsonAtribuirAbatimento;
            AJson.Add('alteracaoAbatimento').Assign(JsonPairAtribuirAbatimento);
          finally
            JsonPairAtribuirAbatimento.Free;
          end;

        end;
      finally
        JsonAtribuirAbatimento.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AtribuirJuros(AJson: TJsonObject);
var
  JsonAtribuirJuros: TJsonObject;
  JsonPairAtribuirJuros: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirJuros := TJSONObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          JsonAtribuirJuros.Add('tipoJuros').Value.AsInteger := (StrToIntDef(ATitulo.CodigoMora, 3));

          case (StrToIntDef(ATitulo.CodigoMora, 2)) of
            1 : JsonAtribuirJuros.Add('valorJuros').Value.AsNumber:= ATitulo.ValorMoraJuros;
            2 : JsonAtribuirJuros.Add('taxaJuros').Value.AsNumber := ATitulo.ValorMoraJuros;
          end;

          JsonPairAtribuirJuros := TJsonPair.Create(AJson, 'juros');
          try
            JsonPairAtribuirJuros.Value.AsObject := JsonAtribuirJuros;
            AJson.Add('juros').Assign(JsonPairAtribuirJuros);
          finally
            JsonPairAtribuirJuros.Free;
          end;

        end;
      finally
        JsonAtribuirJuros.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AtribuirMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: integer;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJSONObject.Create;

      try
        if ATitulo.PercentualMulta > 0 then
        begin
          if ATitulo.MultaValorFixo then
            ACodMulta := 1
          else
            ACodMulta := 2;
        end
        else
          ACodMulta := 3;


        if (ATitulo.DataMulta > 0) then
        begin
          JsonMulta.Add('tipoMulta').Value.AsInteger        := ACodMulta;
          JsonMulta.Add('dataInicioMulta').Value.AsString   := FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY');
          case ACodMulta of
            1 : JsonMulta.Add('valorMulta').Value.AsNumber := ATitulo.ValorMoraJuros;
            2 : JsonMulta.Add('taxaMulta').Value.AsNumber  := ATitulo.PercentualMulta;
          end;

          JsonPairMulta := TJsonPair.Create(AJson, 'multa');
          try
            JsonPairMulta.Value.AsObject := JsonMulta;
            AJson.Add('multa').Assign(JsonPairMulta);
          finally
            JsonPairMulta.Free;
          end;
        end;
      finally
        JsonMulta.Free;
      end;
    end;
  end;

end;

procedure TBoletoW_BancoBrasil_API.AtribuirNegativacao(AJson: TJsonObject);
var
  JsonAtribuirNegativacao: TJsonObject;
  JsonPairAtribuirNegativacao: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirNegativacao := TJSONObject.Create;
      try
        if (ATitulo.DiasDeNegativacao > 0) then
        begin
          JsonAtribuirNegativacao.Add('quantidadeDiasNegativacao').Value.AsInteger := ATitulo.DiasDeNegativacao;
          JsonAtribuirNegativacao.Add('tipoNegativacao').Value.AsInteger := 1;

          JsonPairAtribuirNegativacao := TJsonPair.Create(AJson, 'negativacao');
          try
            JsonPairAtribuirNegativacao.Value.AsObject := JsonAtribuirNegativacao;
            AJson.Add('negativacao').Assign(JsonPairAtribuirNegativacao);
          finally
            JsonPairAtribuirNegativacao.Free;
          end;

        end;
      finally
        JsonAtribuirNegativacao.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoSeuNumero(AJson: TJsonObject);
var
  JsonAtribuirSeuNumero: TJsonObject;
  JsonPairAtribuirSeuNumero: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirSeuNumero := TJSONObject.Create;
      try
        if (ATitulo.SeuNumero <> '') then
        begin
          JsonAtribuirSeuNumero.Add('codigoSeuNumero').Value.AsString := ATitulo.SeuNumero;

          JsonPairAtribuirSeuNumero := TJsonPair.Create(AJson, 'alteracaoSeuNumero');
          try
            JsonPairAtribuirSeuNumero.Value.AsObject := JsonAtribuirSeuNumero;
            AJson.Add('alteracaoSeuNumero').Assign(JsonPairAtribuirSeuNumero);
          finally
            JsonPairAtribuirSeuNumero.Free;
          end;

        end;
      finally
        JsonAtribuirSeuNumero.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoEnderecoPagador(AJson: TJsonObject);
var
  JsonAtribuirEndereco: TJsonObject;
  JsonPairAtribuirEndereco: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirEndereco := TJSONObject.Create;
      try
        if (ATitulo.SeuNumero <> '') then
        begin
          JsonAtribuirEndereco.Add('enderecoPagador').Value.AsString := ATitulo.Sacado.Logradouro;
          JsonAtribuirEndereco.Add('bairroPagador').Value.AsString := ATitulo.Sacado.Bairro;
          JsonAtribuirEndereco.Add('cidadePagador').Value.AsString := ATitulo.Sacado.Cidade;
          JsonAtribuirEndereco.Add('UFPagador').Value.AsString := ATitulo.Sacado.UF;
          JsonAtribuirEndereco.Add('CEPPagador').Value.AsString := ATitulo.Sacado.CEP;

          JsonPairAtribuirEndereco := TJsonPair.Create(AJson, 'alteracaoEndereco');
          try
            JsonPairAtribuirEndereco.Value.AsObject := JsonAtribuirEndereco;
            AJson.Add('alteracaoEndereco').Assign(JsonPairAtribuirEndereco);
          finally
            JsonPairAtribuirEndereco.Free;
          end;

        end;
      finally
        JsonAtribuirEndereco.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_BancoBrasil_API.AlteracaoPrazo(AJson: TJsonObject);
var
  JsonAtribuirAlteracaoPrazo: TJsonObject;
  JsonPairAtribuirAlteracaoPrazo: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonAtribuirAlteracaoPrazo := TJSONObject.Create;
      try
        if (ATitulo.SeuNumero <> '') then
        begin
          JsonAtribuirAlteracaoPrazo.Add('quantidadeDiasAceite').Value.AsInteger := DaysBetween(ATitulo.Vencimento, ATitulo.DataLimitePagto);

          JsonPairAtribuirAlteracaoPrazo := TJsonPair.Create(AJson, 'alteracaoPrazo');
          try
            JsonPairAtribuirAlteracaoPrazo.Value.AsObject := JsonAtribuirAlteracaoPrazo;
            AJson.Add('alteracaoPrazo').Assign(JsonPairAtribuirAlteracaoPrazo);
          finally
            JsonPairAtribuirAlteracaoPrazo.Free;
          end;

        end;
      finally
        JsonAtribuirAlteracaoPrazo.Free;
      end;
    end;

  end;

end;

function TBoletoW_BancoBrasil_API.CodigoTipoTitulo(AEspecieDoc : String): Integer;
begin
{ Pegando o tipo de EspecieDoc }
  if AEspecieDoc = 'CH' then
    AEspecieDoc   := '01'
  else if AEspecieDoc = 'DM' then
    AEspecieDoc   := '02'
  else if AEspecieDoc = 'DMI' then
    AEspecieDoc   := '03'
  else if AEspecieDoc = 'DS' then
    AEspecieDoc   := '04'
  else if AEspecieDoc = 'DSI' then
    AEspecieDoc   := '05'
  else if AEspecieDoc = 'DR' then
    AEspecieDoc   := '06'
  else if AEspecieDoc = 'LC' then
    AEspecieDoc   := '07'
  else if AEspecieDoc = 'NCC' then
    AEspecieDoc   := '08'
  else if AEspecieDoc = 'NCE' then
    AEspecieDoc   := '09'
  else if AEspecieDoc = 'NCI' then
    AEspecieDoc   := '10'
  else if AEspecieDoc = 'NCR' then
    AEspecieDoc   := '11'
  else if AEspecieDoc = 'NP' then
    AEspecieDoc   := '12'
  else if AEspecieDoc = 'NPR' then
    AEspecieDoc   := '13'
  else if AEspecieDoc = 'TM' then
    AEspecieDoc   := '14'
  else if AEspecieDoc = 'TS' then
    AEspecieDoc   := '15'
  else if AEspecieDoc = 'NS' then
    AEspecieDoc   := '16'
  else if AEspecieDoc = 'RC' then
    AEspecieDoc   := '17'
  else if AEspecieDoc = 'FAT' then
    AEspecieDoc   := '18'
  else if AEspecieDoc = 'ND' then
    AEspecieDoc   := '19'
  else if AEspecieDoc = 'AP' then
    AEspecieDoc   := '20'
  else if AEspecieDoc = 'ME' then
    AEspecieDoc   := '21'
  else if AEspecieDoc = 'PC' then
    AEspecieDoc   := '22';
  Result := StrToIntDef(AEspecieDoc,0);
end;

constructor TBoletoW_BancoBrasil_API.Create(ABoletoWS: TBoletoWS);
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

function TBoletoW_BancoBrasil_API.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;

end;

function TBoletoW_BancoBrasil_API.Enviar: boolean;
begin
  Result := inherited Enviar;

end;
end.
