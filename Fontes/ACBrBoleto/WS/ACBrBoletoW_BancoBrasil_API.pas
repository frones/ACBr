{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales - Pandaaa}
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
  ACBrUtil.Strings,
  ACBrJSON;


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
    procedure GerarPagador(AJsonObject: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJsonObject: TACBrJSONObject);
    procedure GerarJuros(AJsonObject: TACBrJSONObject);
    procedure GerarMulta(AJsonObject: TACBrJSONObject);
    procedure GerarDesconto(AJsonObject: TACBrJSONObject);

    procedure AlteraDataVencimento(AJsonObject: TACBrJSONObject);
    procedure AtribuirDesconto(AJsonObject: TACBrJSONObject);
    procedure AlteracaoDesconto(AJsonObject: TACBrJSONObject);
    procedure AlteracaoDataDesconto(AJsonObject: TACBrJSONObject);
    procedure AlterarProtesto(AJsonObject: TACBrJSONObject);
    procedure AtribuirAbatimento(AJsonObject: TACBrJSONObject);
    procedure AlteracaoAbatimento(AJsonObject: TACBrJSONObject);
    procedure AtribuirJuros(AJsonObject: TACBrJSONObject);
    procedure AtribuirMulta(AJsonObject: TACBrJSONObject);
    procedure AtribuirNegativacao(AJsonObject: TACBrJSONObject);
    procedure AlteracaoSeuNumero(AJsonObject: TACBrJSONObject);
    procedure AlteracaoEnderecoPagador(AJsonObject: TACBrJSONObject);
    procedure AlteracaoPrazo(AJsonObject: TACBrJSONObject);

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

  CHARS_VALIDOS : TSetOfChars = ['A'..'Z','0'..'9',
                                 ' ','-','.',
                                 'À','Á','Â','Ã','Ä','Å',
                                 'È','É','Ê','Ë',
                                 'Ì','Í','Î','Ï',
                                 'Ò','Ó','Ô','Õ','Ö',
                                 'Ù','Ú','Û','Ü'];
implementation

uses
  synacode,
  strutils,
  DateUtils,
  ACBrUtil.DateTime;

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
end;

procedure TBoletoW_BancoBrasil_API.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';
end;

function TBoletoW_BancoBrasil_API.DefinirParametros: String;
var
  LConsulta : TStringList;
  LDocumento : String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
      if Boleto.Configuracoes.WebService.Filtro.indicadorSituacao = isbNenhum then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o indicadorSituacao diferente de isbNenhum. ');

      if (Boleto.Cedente.Agencia = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o agenciaBeneficiario. ');

      if (Boleto.Cedente.Conta = EmptyStr) then
        raise EACBrBoletoWSException.Create(ClassName + ' Obrigatório informar o contaBeneficiario. ');

      LDocumento := OnlyNumber(Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

      LConsulta := TStringList.Create;
      try
        LConsulta.Delimiter := '&';
        LConsulta.Add('indicadorSituacao='+IfThen(Boleto.Configuracoes.WebService.Filtro.indicadorSituacao = isbBaixado,'B','A'));

        if Boleto.Configuracoes.WebService.Filtro.contaCaucao > 0 then
          LConsulta.Add('contaCaucao='+ IntToStr(Boleto.Configuracoes.WebService.Filtro.contaCaucao));

        LConsulta.Add('agenciaBeneficiario='+OnlyNumber( Boleto.Cedente.Agencia ));
        LConsulta.Add('contaBeneficiario='+OnlyNumber( Boleto.Cedente.Conta ));

        if Boleto.Configuracoes.WebService.Filtro.carteira > 0 then
          LConsulta.Add('carteiraConvenio='+IntToStr(Boleto.Configuracoes.WebService.Filtro.carteira));

        if Boleto.Configuracoes.WebService.Filtro.carteiraVariacao > 0 then
          LConsulta.Add('variacaoCarteiraConvenio='+IntToStr(Boleto.Configuracoes.WebService.Filtro.carteiraVariacao));

        if Boleto.Configuracoes.WebService.Filtro.modalidadeCobranca > 0 then
          LConsulta.Add('modalidadeCobranca='+ IntToStr(Boleto.Configuracoes.WebService.Filtro.modalidadeCobranca));

        if Length(LDocumento) = 14 then
        begin
          LConsulta.Add('cnpjPagador='+Copy(LDocumento,1,12));
          LConsulta.Add('digitoCNPJPagador='+Copy(LDocumento,13,2));
        end else
        if Length(LDocumento) = 11 then
        begin
          LConsulta.Add('cpfPagador='+Copy(LDocumento,1,9));
          LConsulta.Add('digitoCPFPagador='+Copy(LDocumento,10,2));
        end;

        if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
          LConsulta.Add('dataInicioVencimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal > 0 then
          LConsulta.Add('dataFimVencimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0 then
          LConsulta.Add('dataInicioRegistro='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal > 0 then
          LConsulta.Add('dataFimRegistro='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio > 0 then
          LConsulta.Add('dataInicioMovimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio, 'DD.MM.YYYY'));
        if Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal > 0 then
          LConsulta.Add('dataFimMovimento='+FormatDateBr(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal, 'DD.MM.YYYY'));

        if Boleto.Configuracoes.WebService.Filtro.codigoEstadoTituloCobranca > 0 then
          LConsulta.Add('codigoEstadoTituloCobranca='+intToStr(Boleto.Configuracoes.WebService.Filtro.codigoEstadoTituloCobranca));

        if not (Boleto.Configuracoes.WebService.Filtro.boletoVencido = ibvNenhum) then
          LConsulta.Add('boletoVencido='+IfThen(Boleto.Configuracoes.WebService.Filtro.boletoVencido = ibvSim,'S','N'));

        if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
          LConsulta.Add('indice='+ FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade));

      finally
        Result := LConsulta.DelimitedText;
        LConsulta.Free;
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
  LData: string;
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));
      LJsonObject.AddPair('numeroCarteira', StrToIntDef(OnlyNumber(ATitulo.Carteira),0));
      LJsonObject.AddPair('numeroVariacaoCarteira', StrToIntDef(OnlyNumber(Boleto.Cedente.Modalidade),0));

      if Boleto.Cedente.CaracTitulo = tcVinculada then
        LJsonObject.AddPair('codigoModalidade', 4)
      else
        LJsonObject.AddPair('codigoModalidade', 1);

      LJsonObject.AddPair('dataEmissao', FormatDateBr(ATitulo.DataDocumento, 'DD.MM.YYYY'));
      LJsonObject.AddPair('dataVencimento', FormatDateBr(ATitulo.Vencimento, 'DD.MM.YYYY'));
      LJsonObject.AddPair('valorOriginal', ATitulo.ValorDocumento);
      LJsonObject.AddPair('valorAbatimento', ATitulo.ValorAbatimento);

      if (ATitulo.DataProtesto > 0) then
        LJsonObject.AddPair('quantidadeDiasProtesto', Trunc(ATitulo.DataProtesto - ATitulo.Vencimento))
      else
        LJsonObject.AddPair('quantidadeDiasProtesto', 0);

      if (ATitulo.DataLimitePagto > 0 ) then
      begin
        LJsonObject.AddPair('indicadorAceiteTituloVencido', 'S');
        LJsonObject.AddPair('numeroDiasLimiteRecebimento', Trunc(ATitulo.DataLimitePagto - ATitulo.Vencimento));
      end;

      LJsonObject.AddPair('codigoAceite', IfThen(ATitulo.Aceite = atSim,'A','N'));
      LJsonObject.AddPair('codigoTipoTitulo', codigoTipoTitulo(ATitulo.EspecieDoc));
      LJsonObject.AddPair('descricaoTipoTitulo', ATitulo.EspecieDoc);

      if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
        LJsonObject.AddPair('indicadorPermissaoRecebimentoParcial', 'S');

      LJsonObject.AddPair('numeroTituloBeneficiario', Copy(Trim(UpperCase(ATitulo.NumeroDocumento)),0,15));

      LJsonObject.AddPair('campoUtilizacaoBeneficiario', Trim(Copy(OnlyCharsInSet(AnsiUpperCase(ATitulo.Mensagem.Text),CHARS_VALIDOS),0,30)));
      LJsonObject.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));
      LJsonObject.AddPair('mensagemBloquetoOcorrencia', UpperCase(Copy(Trim(ATitulo.Instrucao1 +' '+ATitulo.Instrucao2+' '+ATitulo.Instrucao3),0,165)));

      GerarDesconto( LJsonObject );
      GerarJuros( LJsonObject );
      GerarMulta( LJsonObject );
      GerarPagador( LJsonObject );
      GerarBenificiarioFinal( LJsonObject );

      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        LJsonObject.AddPair('quantidadeDiasNegativacao', ATitulo.DiasDeNegativacao);
        LJsonObject.AddPair('orgaoNegativador', StrToInt64Def(ATitulo.orgaoNegativador,0));
      end;

      LJsonObject.AddPair('indicadorPix', IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'S','N'));

      LData := LJsonObject.ToJSON;

      FPDadosMsg := LData;

    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXCancelar;
var
  LData: string;
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));
      LData := LJsonObject.ToJSON;
      FPDadosMsg := LData;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXConsultar;
begin
  //Sem Payload - Define Método GET
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoPIXCriar;
var
  LData: string;
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));
      LData := LJsonObject.ToJSON;
      FPDadosMsg := LData;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoAltera;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3:  // RemessaConcederAbatimento
          begin
            LJsonObject.AddPair('indicadorIncluirAbatimento', 'S');
            AtribuirAbatimento(LJsonObject);
          end;
        4:  // RemessaCancelarAbatimento
          begin
            LJsonObject.AddPair('indicadorAlterarAbatimento', 'S');
            AlteracaoAbatimento(LJsonObject);
          end;
        5: //RemessaConcederDesconto
          begin
            LJsonObject.AddPair('indicadorAtribuirDesconto', 'S');
            AtribuirDesconto(LJsonObject);
          end;
        7: //RemessaAlterarVencimento
          begin
            LJsonObject.AddPair('indicadorNovaDataVencimento', 'S');
            AlteraDataVencimento(LJsonObject);
          end;
        9:  //RemessaProtestar
          begin
            LJsonObject.AddPair('indicadorProtestar', 'S');
            AlterarProtesto(LJsonObject);
          end;
        10:  //RemessaSustarProtesto
          begin
            LJsonObject.AddPair('indicadorSustacaoProtesto', 'S');
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            LJsonObject.AddPair('indicadorCancelarProtesto', 'S');
          end;
        13:  //RemessaDispensarJuros
          begin
            LJsonObject.AddPair('indicadorDispensarJuros', 'S');
          end;
        14:  //RemessaAlterarNomeEnderecoSacado
          begin
            LJsonObject.AddPair('indicadorAlterarEnderecoPagador', 'S');
            AlteracaoEnderecoPagador(LJsonObject);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            LJsonObject.AddPair('indicadorAlterarSeuNumero', 'S');
            AlteracaoSeuNumero(LJsonObject);
          end;
        37: //RemessaCobrarJurosMora
          begin
            LJsonObject.AddPair('indicadorCobrarJuros', 'S');
            AtribuirJuros(LJsonObject);
          end;
        50:  //RemessaAlterarMulta
          begin
            LJsonObject.AddPair('indicadorCobrarMulta', 'S');
            AtribuirMulta(LJsonObject);
          end;
        51:  //RemessaDispensarMulta
          begin
            LJsonObject.AddPair('indicadorDispensarMulta', 'S');
          end;
        52: //RemessaAlterarDesconto
          begin
            LJsonObject.AddPair('indicadorAlterarDesconto', 'S');
            AlteracaoDesconto(LJsonObject);
          end;
        53: //RemessaAlterarDataDesconto
          begin
            LJsonObject.AddPair('indicadorAlterarDataDesconto', 'S');
            AlteracaoDataDesconto(LJsonObject);
          end;
        55:  //RemessaAlterarPrazoLimiteRecebimento
          begin
            LJsonObject.AddPair('indicadorAlterarPrazoBoletoVencido', 'S');
            AlteracaoPrazo(LJsonObject);
          end;
        66:  //RemessaNegativacaoSemProtesto
          begin
            LJsonObject.AddPair('indicadorNegativar', 'S');
            AtribuirNegativacao(LJsonObject);
          end;
      end;

      FPDadosMsg := LJsonObject.ToJSON;

    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.RequisicaoBaixa;
var
  LData: string;
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));
      LData := LJsonObject.ToJSON;

      FPDadosMsg := LData;

    finally
      LJsonObject.Free;
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

procedure TBoletoW_BancoBrasil_API.GerarPagador(AJsonObject: TACBrJSONObject);
var
  LJsonPagadorObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonPagadorObject := TACBrJSONObject.Create;

    LJsonPagadorObject.AddPair('tipoInscricao', StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2')));
    LJsonPagadorObject.AddPair('numeroInscricao', StrToInt64(OnlyNumber(ATitulo.Sacado.CNPJCPF)));
    LJsonPagadorObject.AddPair('nome', ATitulo.Sacado.NomeSacado);
    LJsonPagadorObject.AddPair('endereco', ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero);
    LJsonPagadorObject.AddPair('cep', StrToInt(OnlyNumber(ATitulo.Sacado.CEP)));
    LJsonPagadorObject.AddPair('cidade', ATitulo.Sacado.Cidade);
    LJsonPagadorObject.AddPair('bairro', ATitulo.Sacado.Bairro);
    LJsonPagadorObject.AddPair('uf', ATitulo.Sacado.UF);

    AJsonObject.AddPair('pagador', LJsonPagadorObject);

  end;

end;

procedure TBoletoW_BancoBrasil_API.GerarBenificiarioFinal(AJsonObject: TACBrJSONObject);
var
  LJsonSacadorAvalista: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;
    LJsonSacadorAvalista := TACBrJSONObject.Create;

    LJsonSacadorAvalista.AddPair('tipoInscricao', StrToInt(IfThen( Length( OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,'1','2')));
    LJsonSacadorAvalista.AddPair('numeroInscricao', StrToInt64Def(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF),0));
    LJsonSacadorAvalista.AddPair('nome', ATitulo.Sacado.SacadoAvalista.NomeAvalista);

    AJsonObject.AddPair('beneficiarioFinal', LJsonSacadorAvalista);
  end;
end;

procedure TBoletoW_BancoBrasil_API.GerarJuros(AJsonObject: TACBrJSONObject);
var
  LJsonJurosObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonJurosObject := TACBrJSONObject.Create;
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

      LJsonJurosObject.AddPair('tipo', StrToIntDef(ATitulo.CodigoMora, 3));
      case (StrToIntDef(ATitulo.CodigoMora, 3)) of
        1 : LJsonJurosObject.AddPair('valor', ATitulo.ValorMoraJuros);
        2 : LJsonJurosObject.AddPair('porcentagem', ATitulo.ValorMoraJuros);
      end;

      AJsonObject.AddPair('jurosMora', LJsonJurosObject);

    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.GerarMulta(AJsonObject: TACBrJSONObject);
var
  LJsonMultaObject: TACBrJSONObject;
  LCodigoMulta: Byte;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonMultaObject := TACBrJSONObject.Create;

    if ATitulo.PercentualMulta > 0 then
    begin
      if ATitulo.MultaValorFixo then
        LCodigoMulta := 1
      else
        LCodigoMulta := 2;
    end
    else
      LCodigoMulta := 0;

    if (ATitulo.DataMulta > 0) then
    begin
      LJsonMultaObject.AddPair('tipo', LCodigoMulta);

      if( LCodigoMulta > 0 ) then
        LJsonMultaObject.AddPair('data', FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY'));

      case LCodigoMulta of
        1 : LJsonMultaObject.AddPair('valor', ATitulo.PercentualMulta);
        2 : LJsonMultaObject.AddPair('porcentagem', ATitulo.PercentualMulta);
      end;

      AJsonObject.AddPair('multa',LJsonMultaObject);

    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.GerarDesconto(AJsonObject: TACBrJSONObject);
var
  LJsonDescontoObject, LJsonDescontoObject2: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonDescontoObject := TACBrJSONObject.Create;
    if (ATitulo.DataDesconto > 0) then
    begin
      LJsonDescontoObject.AddPair('tipo', integer(ATitulo.TipoDesconto));
      LJsonDescontoObject.AddPair('dataExpiracao', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));
      case integer(ATitulo.TipoDesconto) of
        1 : LJsonDescontoObject.AddPair('valor', ATitulo.ValorDesconto);
        2 : LJsonDescontoObject.AddPair('porcentagem', ATitulo.ValorDesconto);
      end;

      AJsonObject.AddPair('desconto', LJsonDescontoObject);

    end;

    LJsonDescontoObject2 := TACBrJSONObject.Create;
    LJsonDescontoObject2.AddPair('tipo', integer(ATitulo.TipoDesconto));
    if ATitulo.DataDesconto2 > 0 then
    begin
      LJsonDescontoObject2.AddPair('dataExpiracao', FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY'));
      case integer(ATitulo.TipoDesconto2) of
        1 : LJsonDescontoObject2.AddPair('valor', ATitulo.ValorDesconto2);
        2 : LJsonDescontoObject2.AddPair('porcentagem', ATitulo.ValorDesconto2);
      end;

      AJsonObject.AddPair('segundoDesconto', LJsonDescontoObject2);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteraDataVencimento(AJsonObject: TACBrJSONObject);
var
  LJsonVencimentoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonVencimentoObject := TACBrJSONObject.Create;
    if (ATitulo.Vencimento > 0) then
    begin
      LJsonVencimentoObject.AddPair('novaDataVencimento', FormatDateBr(ATitulo.Vencimento, 'DD.MM.YYYY'));

      AJsonObject.AddPair('alteracaoData', LJsonVencimentoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AtribuirDesconto(AJsonObject: TACBrJSONObject);
var
  LJsonAtribuirDescontoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAtribuirDescontoObject := TACBrJSONObject.Create;
    if (ATitulo.ValorDesconto > 0) then
    begin
      LJsonAtribuirDescontoObject.AddPair('tipoPrimeiroDesconto', integer(ATitulo.TipoDesconto));
      case integer(ATitulo.TipoDesconto) of
        1 : LJsonAtribuirDescontoObject.AddPair('valorPrimeiroDesconto', ATitulo.ValorDesconto);
        2 : LJsonAtribuirDescontoObject.AddPair('percentualPrimeiroDesconto', ATitulo.ValorDesconto);
      end;
      LJsonAtribuirDescontoObject.AddPair('dataPrimeiroDesconto', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));
    end;
    if (ATitulo.ValorDesconto2 > 0) then
    begin
      LJsonAtribuirDescontoObject.AddPair('tipoSegundoDesconto', integer(ATitulo.TipoDesconto2));
      case integer(ATitulo.TipoDesconto2) of
        1 : LJsonAtribuirDescontoObject.AddPair('valorSegundoDesconto', ATitulo.ValorDesconto2);
        2 : LJsonAtribuirDescontoObject.AddPair('percentualSegundoDesconto', ATitulo.ValorDesconto2);
      end;
      LJsonAtribuirDescontoObject.AddPair('dataSegundoDesconto', FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY'));
    end;

    if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
    begin
      AJsonObject.AddPair('desconto', LJsonAtribuirDescontoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoDesconto(AJsonObject: TACBrJSONObject);
var
  LJsonAtribuirDescontoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAtribuirDescontoObject := TACBrJSONObject.Create;
    try
      if (ATitulo.ValorDesconto > 0) then
      begin
        LJsonAtribuirDescontoObject.AddPair('tipoPrimeiroDesconto', integer(ATitulo.TipoDesconto));
        case integer(ATitulo.TipoDesconto) of
          1 : LJsonAtribuirDescontoObject.AddPair('novoValorPrimeiroDesconto', ATitulo.ValorDesconto);
          2 : LJsonAtribuirDescontoObject.AddPair('novoPercentualPrimeiroDesconto', ATitulo.ValorDesconto);
        end;
        LJsonAtribuirDescontoObject.AddPair('novaDataLimitePrimeiroDesconto', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));
      end;
      if (ATitulo.ValorDesconto2 > 0) then
      begin
        LJsonAtribuirDescontoObject.AddPair('tipoSegundoDesconto', integer(ATitulo.TipoDesconto2));
        case integer(ATitulo.TipoDesconto2) of
          1 : LJsonAtribuirDescontoObject.AddPair('novoValorSegundoDesconto', ATitulo.ValorDesconto2);
          2 : LJsonAtribuirDescontoObject.AddPair('novoPercentualSegundoDesconto', ATitulo.ValorDesconto2);
        end;
        LJsonAtribuirDescontoObject.AddPair('novaDataLimiteSegundoDesconto', FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY'));
      end;

      if (ATitulo.ValorDesconto > 0) or (ATitulo.ValorDesconto > 0) then
        AJsonObject.AddPair('alteracaoDesconto',LJsonAtribuirDescontoObject);

    finally
      LJsonAtribuirDescontoObject.Free;
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoDataDesconto(AJsonObject: TACBrJSONObject);
var
  LJsonAlteracaoDataDescontoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAlteracaoDataDescontoObject := TACBrJSONObject.Create;

    if (ATitulo.DataDesconto > 0) then
      LJsonAlteracaoDataDescontoObject.AddPair('novaDataLimitePrimeiroDesconto', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));

    if (ATitulo.DataDesconto2 > 0) then
      LJsonAlteracaoDataDescontoObject.AddPair('novaDataLimiteSegundoDesconto', FormatDateBr(ATitulo.DataDesconto2, 'DD.MM.YYYY'));


    if (ATitulo.DataDesconto > 0) or (ATitulo.DataDesconto2 > 0) then
      AJsonObject.AddPair('alteracaoDataDesconto', LJsonAlteracaoDataDescontoObject);
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlterarProtesto(AJsonObject: TACBrJSONObject);
var
  LJsonAlterarProtestoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAlterarProtestoObject := TACBrJSONObject.Create;
    if (ATitulo.DiasDeProtesto > 0) then
    begin
      LJsonAlterarProtestoObject.AddPair('quantidadeDiasProtesto', ATitulo.DiasDeProtesto);
      AJsonObject.AddPair('protesto', LJsonAlterarProtestoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AtribuirAbatimento(AJsonObject: TACBrJSONObject);
var
  LJsonAtribuirAbatimentoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAtribuirAbatimentoObject := TACBrJSONObject.Create;
    if (ATitulo.ValorAbatimento > 0) then
    begin
      LJsonAtribuirAbatimentoObject.AddPair('valorAbatimento', ATitulo.ValorAbatimento);
      AJsonObject.AddPair('abatimento',LJsonAtribuirAbatimentoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoAbatimento(AJsonObject: TACBrJSONObject);
var
  LJsonAlteracaoAbatimentoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAlteracaoAbatimentoObject := TACBrJSONObject.Create;
    if (ATitulo.ValorAbatimento > 0) then
    begin
      LJsonAlteracaoAbatimentoObject.AddPair('novoValorAbatimento', ATitulo.ValorAbatimento);
      AJsonObject.AddPair('alteracaoAbatimento',LJsonAlteracaoAbatimentoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AtribuirJuros(AJsonObject: TACBrJSONObject);
var
  JsonAtribuirJuros: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    JsonAtribuirJuros := TACBrJSONObject.Create;
    if (ATitulo.ValorMoraJuros > 0) then
    begin
      JsonAtribuirJuros.AddPair('tipoJuros', (StrToIntDef(ATitulo.CodigoMora, 3)));

      case (StrToIntDef(ATitulo.CodigoMora, 2)) of
        1 : JsonAtribuirJuros.AddPair('valorJuros', ATitulo.ValorMoraJuros);
        2 : JsonAtribuirJuros.AddPair('taxaJuros', ATitulo.ValorMoraJuros);
      end;
      AJsonObject.AddPair('juros', JsonAtribuirJuros);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AtribuirMulta(AJsonObject: TACBrJSONObject);
var
  LJsonMultaObject: TACBrJSONObject;
  LCodigoMulta: Byte;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonMultaObject := TACBrJSONObject.Create;

    if ATitulo.PercentualMulta > 0 then
    begin
      if ATitulo.MultaValorFixo then
        LCodigoMulta := 1
      else
        LCodigoMulta := 2;
    end
    else
      LCodigoMulta := 3;


    if (ATitulo.DataMulta > 0) then
    begin
      LJsonMultaObject.AddPair('tipoMulta', LCodigoMulta);
      LJsonMultaObject.AddPair('dataInicioMulta', FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY'));
      case LCodigoMulta of
        1 : LJsonMultaObject.AddPair('valorMulta', ATitulo.ValorMoraJuros);
        2 : LJsonMultaObject.AddPair('taxaMulta', ATitulo.PercentualMulta);
      end;

      AJsonObject.AddPair('multa', LJsonMultaObject);

    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AtribuirNegativacao(AJsonObject: TACBrJSONObject);
var
  LJsonAtribuirNegativacaoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAtribuirNegativacaoObject := TACBrJSONObject.Create;
    if (ATitulo.DiasDeNegativacao > 0) then
    begin
      LJsonAtribuirNegativacaoObject.AddPair('quantidadeDiasNegativacao', ATitulo.DiasDeNegativacao);
      LJsonAtribuirNegativacaoObject.AddPair('tipoNegativacao', 1);

      AJsonObject.AddPair('negativacao', LJsonAtribuirNegativacaoObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoSeuNumero(AJsonObject: TACBrJSONObject);
var
  LJsonAlteracaoSeuNumeroObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject)  then
  begin
    LJsonAlteracaoSeuNumeroObject := TACBrJSONObject.Create;
    if (ATitulo.SeuNumero <> '') then
    begin
      LJsonAlteracaoSeuNumeroObject.AddPair('codigoSeuNumero', ATitulo.SeuNumero);
      AJsonObject.AddPair('alteracaoSeuNumero', LJsonAlteracaoSeuNumeroObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoEnderecoPagador(AJsonObject: TACBrJSONObject);
var
  LJsonAlteracaoEnderecoPagadorObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAlteracaoEnderecoPagadorObject := TACBrJSONObject.Create;
    if (ATitulo.SeuNumero <> '') then
    begin
      LJsonAlteracaoEnderecoPagadorObject.AddPair('enderecoPagador', ATitulo.Sacado.Logradouro);
      LJsonAlteracaoEnderecoPagadorObject.AddPair('bairroPagador', ATitulo.Sacado.Bairro);
      LJsonAlteracaoEnderecoPagadorObject.AddPair('cidadePagador', ATitulo.Sacado.Cidade);
      LJsonAlteracaoEnderecoPagadorObject.AddPair('UFPagador', ATitulo.Sacado.UF);
      LJsonAlteracaoEnderecoPagadorObject.AddPair('CEPPagador',  ATitulo.Sacado.CEP);
      AJsonObject.AddPair('alteracaoEndereco', LJsonAlteracaoEnderecoPagadorObject);
    end;
  end;
end;

procedure TBoletoW_BancoBrasil_API.AlteracaoPrazo(AJsonObject: TACBrJSONObject);
var
  LJsonAlteracaoPrazoObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJsonObject) then
  begin
    LJsonAlteracaoPrazoObject := TACBrJSONObject.Create;
    try
      if (ATitulo.SeuNumero <> '') then
      begin
        LJsonAlteracaoPrazoObject.AddPair('quantidadeDiasAceite', DaysBetween(ATitulo.Vencimento, ATitulo.DataLimitePagto));
        AJsonObject.AddPair('alteracaoPrazo', LJsonAlteracaoPrazoObject);
      end;
    finally
      LJsonAlteracaoPrazoObject.Free;
    end;
  end;
end;

function TBoletoW_BancoBrasil_API.CodigoTipoTitulo(AEspecieDoc : String): Integer;
begin
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

  Result := StrToIntDef(AEspecieDoc, 0);
end;

constructor TBoletoW_BancoBrasil_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    OAuth.URL := IfThen(OAuth.Ambiente = taHomologacao, C_URL_OAUTH_HOM , C_URL_OAUTH_PROD ) ;
    
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
