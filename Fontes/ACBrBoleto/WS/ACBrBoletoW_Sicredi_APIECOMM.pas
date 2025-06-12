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

unit ACBrBoletoW_Sicredi_APIECOMM;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  ACBrBoletoConversao,
  synacode,
  strutils,
  DateUtils,
  ACBrDFeSSL,
  synautil,
  ACBrBoleto,
  ACBrBoletoWS.Rest,
  ACBrJSON;

type

  { TBoletoW_Sicredi_APIECOMM }
  TBoletoW_Sicredi_APIECOMM = class(TBoletoWSREST)
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
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GeraDadosInstrucao(AJson:TACBrJSONObject );

    procedure AlteraDataVencimento( AJson: TACBrJSONObject);
    procedure AtribuirDesconto(AJson: TACBrJSONObject);
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


{ TBoletoW_Sicredi_APIECOMM }

procedure TBoletoW_Sicredi_APIECOMM.DefinirURL;
var
aNossoNumero  : string;
begin
  if (Boleto.Configuracoes.WebService.Operacao = tpConsultaDetalhe)
  and (ATitulo <> nil) then
    aNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));

  case Boleto.Configuracoes.WebService.Ambiente of
    tawsProducao: FPURL.URLProducao := C_URL;
    tawsHomologacao: FPURL.URLHomologacao := C_URL_HOM;
  end;
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui   : FPURL.SetPathURI( '/emissao' );
    tpConsulta : FPURL.SetPathURI( '/consulta?agencia='+Boleto.Cedente.Agencia
                                + '&posto='+Boleto.Cedente.AgenciaDigito
                                + '&cedente='+Boleto.Cedente.CodigoCedente
                                + '&'
                                + DefinirParametros );
    tpAltera : FPURL.SetPathURI( '/comandoInstrucao' );

    tpConsultaDetalhe : FPURL.SetPathURI( '/consulta?agencia='+Boleto.Cedente.Agencia
                                + '&posto='+Boleto.Cedente.AgenciaDigito
                                + '&cedente='+Boleto.Cedente.CodigoCedente
                                + '&nossoNumero='+aNossoNumero );
    tpBaixa    : FPURL.SetPathURI( '/comandoInstrucao' );
  end;

end;

procedure TBoletoW_Sicredi_APIECOMM.DefinirContentType;
begin
 FPContentType := C_CONTENT_TYPE;
end;


procedure TBoletoW_Sicredi_APIECOMM.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarDados;
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

procedure TBoletoW_Sicredi_APIECOMM.DefinirAuthorization;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Sicredi_APIECOMM.GerarTokenAutenticacao: string;
var
  LJson  : TACBrJSONObject;
  LStream: TMemoryStream;
begin
  LJson := TACBrJsonObject.Create;
  LStream:= TMemoryStream.Create;
  try
      if( not Assigned( FDFeSSL ) ) then
        FDFeSSL := TDFeSSL( Boleto.Configuracoes.WebService);

      FDFeSSL.SSLHttpClass.MimeType := FPContentType;   

      with FDFeSSL.SSLHttpClass.HeaderReq do
        begin
          AddHeader('token', Boleto.Cedente.CedenteWS.ClientID);
        end;

      FDFeSSL.SSLHttpClass.DataReq.LoadFromStream(LStream);
      FDFeSSL.HTTPMethod(MetodoHTTPToStr(htPOST), C_URL_OAUTH_PROD );

      LJson.Parse( UTF8Decode(ReadStrFromStream(FDFeSSL.SSLHttpClass.DataResp, FDFeSSL.SSLHttpClass.DataResp.Size )) );

      if( LJson.AsString['codigo' ] = '' ) then // se não veio código de erro
        begin
          result := LJson.AsString['chaveTransacao'];
        end
      else
        begin
          raise EACBrBoletoWSException.Create(ClassName
                                                  + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO,
                                                            [ 'Código: '
                                                              + '-'
                                                              +LJson.AsString['codigo']
                                                              +#13
                                                              +LJson.AsString['mensagem']
                                                              +#13
                                                              +'Parametro: '
                                                              +LJson.AsString['parametro'] ] ));
        end;
  finally
    LStream.Free;
	  LJson.Free;
    FDFeSSL.SSLHttpClass.HeaderReq.Clear;
    FDFeSSL.SSLHttpClass.HeaderResp.Clear;
    FDFeSSL.SSLHttpClass.Clear;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Sicredi_APIECOMM.DefinirParametros: String;
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

procedure TBoletoW_Sicredi_APIECOMM.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Sicredi_APIECOMM.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao, '1','2'),2);
end;

procedure TBoletoW_Sicredi_APIECOMM.RequisicaoJson;
var
  LJson: TACBrJSONObject;
  LSeuNumero, LNossoNumero:String;
begin
  if Assigned(ATitulo) then
  begin
    LJson := TACBrJsonObject.Create;
    try

      LNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo) );
      LSeuNumero    := ATitulo.NossoNumero;
      LJson.AddPair('agencia', Boleto.Cedente.Agencia);
      LJson.AddPair('posto', Boleto.Cedente.AgenciaDigito);
      LJson.AddPair('cedente', Boleto.Cedente.CodigoCedente);
      LJson.AddPair('especieDocumento', codigoTipoTitulo(ATitulo.EspecieDoc));
      LJson.AddPair('dataVencimento', FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY'));
      LJson.AddPair('valor', ATitulo.ValorDocumento);
      LJson.AddPair('mensagem', UpperCase(Copy(Trim(ATitulo.Instrucao1 +' '+ATitulo.Instrucao2+' '+ATitulo.Instrucao3),0,165)));
      LJson.AddPair('descontoAntecipado', ATitulo.ValorAbatimento);

      GerarDesconto(LJson);
      GerarJuros(LJson);
      GerarMulta(LJson);
      GerarPagador(LJson);
      GerarBenificiarioFinal(LJson);

      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        LJson.AddPair('numDiasNegativacaoAuto',  ATitulo.DiasDeNegativacao);
      end;

      LJson.AddPair('seuNumero', LSeuNumero);
      LJson.AddPair('nossoNumero', LNossoNumero);

      FPDadosMsg := LJson.ToJSON;

    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.RequisicaoAltera;
var
  LJson: TACBrJSONObject;
begin

  if Assigned(ATitulo) then
  begin

    LJson := TACBrJsonObject.Create;

    try
      LJson.AddPair( 'agencia', Boleto.Cedente.Agencia);
      LJson.AddPair( 'posto', Boleto.Cedente.AgenciaDigito);
      LJson.AddPair( 'cedente', Boleto.Cedente.CodigoCedente);
      LJson.AddPair( 'nossoNumero', OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo) ));

      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        3:  // RemessaConcederAbatimento
          begin
            LJson.AddPair( 'instrucaoComando', 'CONCESSAO_ABATIMENTO');
            LJson.AddPair('complementoInstrucao', True);
            AtribuirAbatimento(LJson);
          end;
        4:  // RemessaCancelarAbatimento
          begin
            LJson.AddPair( 'instrucaoComando', 'CANCELAMENTO_ABATIMENTO_CONCEDIDO');
            LJson.AddPair('complementoInstrucao', True);
            AlteracaoAbatimento(LJson);
          end;
        5: //RemessaConcederDesconto
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_OUTROS_DADOS');
            LJson.AddPair( 'complementoInstrucao', 'DESCONTO');
            AtribuirDesconto(LJson);
          end;
        7: //RemessaAlterarVencimento
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_VENCIMENTO');
            LJson.AddPair('complementoInstrucao', True);
            LJson.AddPair('tipoVencimento', 'DATA_ESPECIFICA');
            AlteraDataVencimento(LJson);
          end;
        9:  //RemessaProtestar
          begin
            LJson.AddPair( 'instrucaoComando', 'PEDIDO_PROTESTO');
            LJson.AddPair('complementoInstrucao', True);
            AlterarProtesto(LJson);
          end;
        10:  //RemessaSustarProtesto
          begin
            LJson.AddPair( 'instrucaoComando', 'SUSTAR_PROTESTO_MANTER_CARTEIRA');
            LJson.AddPair('complementoInstrucao', True);
            AlterarProtesto(LJson);
          end;
        12:  //RemessaCancelarInstrucaoProtesto
          begin
            LJson.AddPair( 'instrucaoComando', 'SUSTAR_PROTESTO_BAIXAR_TITULO');
            LJson.AddPair('complementoInstrucao', True);
            AlterarProtesto(LJson);
          end;
        13:  //RemessaDispensarJuros
          begin
            //LJson.AddPair('indicadorDispensarJuros').Value.AsString := 'S';
          end;
        14:  //RemessaAlterarNomeEnderecoSacado
          begin
            //LJson.AddPair('indicadorAlterarEnderecoPagador').Value.AsString := 'S';
            AlteracaoEnderecoPagador(LJson);
          end;
        18:  //RemessaAlterarSeuNumero
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_SEU_NUMERO');
            LJson.AddPair('complementoInstrucao', True);
            AlteracaoSeuNumero(LJson);
          end;
        37: //RemessaCobrarJurosMora
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_OUTROS_DADOS');
            LJson.AddPair('complementoInstrucao', 'JUROS_DIA');
            AtribuirJuros(LJson);
          end;
        50:  //RemessaAlterarMulta
          begin
            AtribuirMulta(LJson);
          end;
        51:  //RemessaDispensarMulta
          begin
            //LJson.AddPair('indicadorDispensarMulta').Value.AsString := 'S';
          end;
        52: //RemessaAlterarDesconto
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_OUTROS_DADOS');
            LJson.AddPair('complementoInstrucao', 'DESCONTO');
            AlteracaoDesconto(LJson);
          end;
        53: //RemessaAlterarDataDesconto
          begin
            LJson.AddPair( 'instrucaoComando', 'ALTERACAO_OUTROS_DADOS');
            LJson.AddPair('complementoInstrucao', 'DATA_LIMITE_CONCESSAO_DESCONTO');
            AlteracaoDataDesconto(LJson);
          end;
        55:  //RemessaAlterarPrazoLimiteRecebimento
          begin
            AlteracaoPrazo(LJson);
          end;
        66:  //RemessaNegativacaoSemProtesto
          begin
            LJson.AddPair('indicadorNegativar', 'S');
            AtribuirNegativacao(LJson);
          end;
      end;
      FPDadosMsg := LJson.ToJson;
    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.GeraDadosInstrucao(AJson: TACBrJSONObject);
var
  LNossoNumero:String;
begin
  LNossoNumero  := OnlyNumber( ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo) );
  AJson.AddPair('agencia', Boleto.Cedente.Agencia);
  AJson.AddPair('posto', Boleto.Cedente.AgenciaDigito);
  AJson.AddPair('cedente', Boleto.Cedente.CodigoCedente);
  AJson.AddPair('nossoNumero', LNossoNumero);
end;

procedure TBoletoW_Sicredi_APIECOMM.RequisicaoBaixa;
var
  LJson: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJson := TACBrJSONObject.Create;
    try
      GeraDadosInstrucao( LJson );
      LJson.AddPair( 'instrucaoComando', 'PEDIDO_BAIXA');
      LJson.AddPair('complementoInstrucao', True);
      FPDadosMsg := LJson.ToJSON;
    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.RequisicaoConsulta;
begin
   //Sem Payload - Define Método GET
end;

procedure TBoletoW_Sicredi_APIECOMM.RequisicaoConsultaDetalhe;
begin
    //Sem Payload - Define Método GET
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarPagador(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    AJson.AddPair('tipoPessoa', StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2')));
    AJson.AddPair('cpfCnpj', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    AJson.AddPair('nome', ATitulo.Sacado.NomeSacado);
    AJson.AddPair('endereco', ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero);
    AJson.AddPair('cep', OnlyNumber(ATitulo.Sacado.CEP));
    AJson.AddPair('cidade', ATitulo.Sacado.Cidade);
    AJson.AddPair('uf', ATitulo.Sacado.UF);
    AJson.AddPair('telefone', IfThen( ATitulo.Sacado.Fone='' , '0',ATitulo.Sacado.Fone ));
    AJson.AddPair('email', ATitulo.Sacado.Email);
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarBenificiarioFinal(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) then
  begin
    if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    if Assigned(AJson) then
    begin
      // No manual não constam dados para informar Avalista
      AJson.AddPair( 'codigoSacadorAvalista', '000');
    end;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarJuros(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    AJson.AddPair( 'tipoJuros', ATitulo.CodigoMora);
    AJson.AddPair( 'juros', ATitulo.ValorMoraJuros);
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarMulta(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.PercentualMulta > 0) then
    AJson.AddPair( 'multas', ATitulo.PercentualMulta);
end;

procedure TBoletoW_Sicredi_APIECOMM.GerarDesconto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    AJson.AddPair('tipoDesconto', ifThen( integer(ATitulo.TipoDesconto)=1,'A' , 'B' ) );
    if (ATitulo.DataDesconto > 0) then
    begin
      AJson.AddPair('valorDesconto1', ATitulo.ValorDesconto);
      AJson.AddPair('dataDesconto1', FormatDateBr(ATitulo.DataDesconto, 'DD/MM/YYYY'));
    end;

    if (ATitulo.DataDesconto2 > 0) then
    begin
      AJson.AddPair('valorDesconto2', ATitulo.ValorDesconto2);
      AJson.AddPair('dataDesconto2', FormatDateBr(ATitulo.DataDesconto2, 'DD/MM/YYYY'));
    end;

    if (ATitulo.DataDesconto3 > 0) then
    begin
      AJson.AddPair('valorDesconto3', ATitulo.ValorDesconto3);
      AJson.AddPair('dataDesconto3', FormatDateBr(ATitulo.DataDesconto3, 'DD/MM/YYYY'));
    end;
  end;
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteraDataVencimento(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('data1', FormatDateBr(ATitulo.Vencimento));
end;

procedure TBoletoW_Sicredi_APIECOMM.AtribuirDesconto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('valor1', ATitulo.ValorDesconto);
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoDesconto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('valor1', ATitulo.ValorDesconto);
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoDataDesconto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('data1', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));
end;

procedure TBoletoW_Sicredi_APIECOMM.AlterarProtesto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('complementoInstrucao', True);
end;

procedure TBoletoW_Sicredi_APIECOMM.AtribuirAbatimento(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('valor1', ATitulo.ValorAbatimento);
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoAbatimento(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('complementoInstrucao', True );
end;

procedure TBoletoW_Sicredi_APIECOMM.AtribuirJuros(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('valor1', ATitulo.ValorMoraJuros);
end;

procedure TBoletoW_Sicredi_APIECOMM.AtribuirMulta(AJson: TACBrJSONObject);
begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_APIECOMM.AtribuirNegativacao(AJson: TACBrJSONObject);
begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoSeuNumero(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) then
    AJson.AddPair('seuNumero', ATitulo.SeuNumero);
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoEnderecoPagador(AJson: TACBrJSONObject);
begin
  //Sem Payload
end;

procedure TBoletoW_Sicredi_APIECOMM.AlteracaoPrazo(AJson: TACBrJSONObject);
begin
// sem Payload
end;

function TBoletoW_Sicredi_APIECOMM.CodigoTipoTitulo(AEspecieDoc : String): String;
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

constructor TBoletoW_Sicredi_APIECOMM.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    case OAuth.Ambiente of
      tawsProducao    : OAuth.URL.URLProducao    := C_URL_OAUTH_PROD;
      tawsHomologacao : OAuth.URL.URLHomologacao := C_URL_OAUTH_HOM;
    end;

    OAuth.Payload := not (OAuth.Ambiente = tawsProducao);
  end;

end;

function TBoletoW_Sicredi_APIECOMM.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;
end;

function TBoletoW_Sicredi_APIECOMM.Enviar: boolean;
begin
  Result := inherited Enviar;
end;

end.
