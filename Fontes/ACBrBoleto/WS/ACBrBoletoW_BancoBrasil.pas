{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior                                }
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

unit ACBrBoletoW_BancoBrasil;

interface

uses
  Classes,
  SysUtils,
  ACBrBoletoWS,
  pcnConversao,
  pcnGerador,
  ACBrBoletoConversao,
  ACBrBoleto,
  ACBrBoletoWS.SOAP;

type

{ TBoletoW_BancoBrasil}

  TBoletoW_BancoBrasil  = class(TBoletoWSSOAP)
  private
    function CodigoTipoTitulo(AEspecieDoc:String): Integer;
  protected

    procedure DefinirEnvelopeSoap; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirRootElement; override;
    procedure DefinirAuthorization; override;

    function GerarTokenAutenticacao: String;

    procedure GerarHeader; override;
    procedure GerarDados; override;

    function DefinirSOAPAtributtes:string; override;
    procedure GerarRequisicao;

    function PrefixTag(AValue: String): String;

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: String; override;
    function Enviar: Boolean; override;

  end;

const
  C_URL = 'https://cobranca.bb.com.br:7101/';
  C_URL_HOM = 'https://cobranca.homologa.bb.com.br:7101/';
  C_URL_OAUTH_HOM = 'https://oauth.hm.bb.com.br/oauth/token';
  C_URL_OAUTH_PROD = 'https://oauth.bb.com.br/oauth/token';
  C_SERVICO_MANUTENCAO = 'registrarBoleto';
  C_SOAP_ATTRIBUTTES = 'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '+
                    'xmlns:sch="http://www.tibco.com/schemas/bws_registro_cbr/Recursos/XSD/Schema.xsd"';
  C_KEYUSER = 'J1234567';
  C_CHANNEL = '5';
  C_PREFIX = 'sch:';

implementation

uses
  synacode, ACBrBoletoPcnConsts, strutils,
  ACBrDFeConsts,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML, ACBrUtil.Base;

{ TBoletoW_BancoBrasil }

procedure TBoletoW_BancoBrasil.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  FPDadosMsg := RemoverDeclaracaoXML(FPDadosMsg);

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Header/>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + FPDadosMsg;
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TBoletoW_BancoBrasil.DefinirURL;
begin
  FPURL := '';
  DefinirServicoEAction;
  DefinirAuthorization;
  FPVersaoServico := Boleto.Configuracoes.WebService.VersaoDF;
end;

procedure TBoletoW_BancoBrasil.DefinirServicoEAction;
var
  Servico: String;
begin
  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:   Servico  := C_SERVICO_MANUTENCAO;
  else
    raise EACBrBoletoWSException.Create(ClassName + Format( S_OPERACAO_NAO_IMPLEMENTADO, [
                                                  TipoOperacaoToStr( Boleto.Configuracoes.WebService.Operacao ) ] ));
  end;

  FPServico := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL, C_URL_HOM) + Servico;
  FPURL := FPServico;

  FPSoapAction := Servico;
end;

function TBoletoW_BancoBrasil.DefinirSOAPAtributtes: string;
begin
  Result := C_SOAP_ATTRIBUTTES;
end;

procedure TBoletoW_BancoBrasil.DefinirRootElement;
begin
  FPRootElement:= '';
  FPCloseRootElement:= '';
end;

procedure TBoletoW_BancoBrasil.DefinirAuthorization;
begin
  FPAuthorization:= C_Authorization + ': ' + 'Bearer ' + GerarTokenAutenticacao;

end;

function TBoletoW_BancoBrasil.GerarTokenAutenticacao:String;
begin
  result:= '';
  if Assigned(OAuth) then
  begin
    OAuth.GrantType := 'client_credentials';
    OAuth.ParamsOAuth := C_GRANT_TYPE
                       + '=' + OAuth.GrantType
                       + '&' + C_SCOPE
                       + '=' + OAuth.Scope;
    if OAuth.GerarToken then
      result := OAuth.Token
    else
      raise EACBrBoletoWSException.Create(ClassName + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO, [OAuth.ErroComunicacao] ));
  end;
end;

procedure TBoletoW_BancoBrasil.GerarHeader;
begin
  //<' + FPSoapVersion + ':Header/>'
end;

procedure TBoletoW_BancoBrasil.GerarDados;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      Gerador.wGrupo(PrefixTag('requisicao'));

      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui: GerarRequisicao;
      else
        raise EACBrBoletoWSException.Create(ClassName + Format( S_OPERACAO_NAO_IMPLEMENTADO, [
                                                  TipoOperacaoToStr( Boleto.Configuracoes.WebService.Operacao ) ] ));
      end;

      Gerador.wGrupo('/'+PrefixTag('requisicao'));
    end;
end;

procedure TBoletoW_BancoBrasil.GerarRequisicao;
var
  lCodMora: Integer;
  lCodigoChaveUsuario: String;
begin
  with ATitulo do
  begin
    Gerador.wCampo(tcStr, '#01', PrefixTag('numeroConvenio'        ), 1, 9, 1, Boleto.Cedente.Convenio , DSC_CONVENIO);
    Gerador.wCampo(tcStr, '#02', PrefixTag('numeroCarteira'        ), 1, 4, 1, Carteira , DSC_CARTEIRA);
    Gerador.wCampo(tcStr, '#03', PrefixTag('numeroVariacaoCarteira'), 1, 4, 1, Boleto.Cedente.Modalidade , DSC_VARIACAO_CARTEIRA);
    Gerador.wCampo(tcStr, '#04', PrefixTag('codigoModalidadeTitulo'), 1, 1, 1, '1' , DSC_CODIGO_MODALIDADE);
    Gerador.wCampo(tcStr, '#05', PrefixTag('dataEmissaoTitulo'     ), 10, 10, 1, FormatDateTime('dd.mm.yyyy', DataDocumento), DSC_DATA_DOCUMENTO);
    Gerador.wCampo(tcStr, '#06', PrefixTag('dataVencimentoTitulo'  ), 10, 10, 1, FormatDateTime('dd.mm.yyyy', Vencimento), DSC_DATA_VENCIMENTO);
    Gerador.wCampo(tcDe2, '#07', PrefixTag('valorOriginalTitulo'   ), 01, 15, 1, ValorDocumento, DSC_VALOR_DOCUMENTO);
    if (ValorDesconto <= 0) then
      Gerador.wCampo(tcStr, '#08', PrefixTag('codigoTipoDesconto'  ), 1, 1, 1, '0' , DSC_TIPO_DESCONTO)
    else
    begin
      case Integer(TipoDesconto) of
        3, 4: begin
          Gerador.wCampo(tcStr, '#08', PrefixTag('codigoTipoDesconto'  ), 1, 1, 1, '3' , DSC_TIPO_DESCONTO);
          //if (DataDesconto > 0) then
            Gerador.wCampo(tcStr, '#09', PrefixTag('dataDescontoTitulo'     ), 10, 10, 1, FormatDateTime('dd.mm.yyyy',DataDesconto), DSC_DATA_DESCONTO);
          Gerador.wCampo(tcDe2, '#10', PrefixTag('valorDescontoTitulo' ), 01, 15, 1, ValorDesconto, DSC_VALOR_DESCONTO);
        end
        else begin
          Gerador.wCampo(tcStr, '#08', PrefixTag('codigoTipoDesconto'  ), 1, 1, 1, '2' , DSC_TIPO_DESCONTO);
          //if (DataDesconto > 0) then
            Gerador.wCampo(tcStr, '#09', PrefixTag('dataDescontoTitulo'     ), 10, 10, 1, FormatDateTime('dd.mm.yyyy',DataDesconto), DSC_DATA_DESCONTO);
          Gerador.wCampo(tcDe4, '#11', PrefixTag('percentualDescontoTitulo'), 01, 15, 1, ValorDesconto, DSC_VALOR_DESCONTO);

        end;
      end;
    end;

    Gerador.wCampo(tcDe2, '#12', PrefixTag('valorAbatimentoTitulo'    ), 01, 15, 1, ValorAbatimento, DSC_VALOR_ABATIMENTO);

    //TK-2079
    if DiasDeProtesto > 0 then
      Gerador.wCampo(tcInt, '#13', PrefixTag('quantidadeDiaProtesto'    ), 01, 03, 1, DiasDeProtesto, DSC_DIAS_PROTESTO);
    //FIM TK-2079

    case (Integer(CodigoMoraJuros) ) of
      0: lCodMora:= 1;
      1: lCodMora:= 2;
      2: lCodMora:= 3;
      else
         lCodMora:= 0;
    end;
    Gerador.wCampo(tcInt, '#14', PrefixTag('codigoTipoJuroMora'    ), 01, 01, 1, lCodMora, DSC_CODIGO_MORA_JUROS);

    if lCodMora = 2 then
      Gerador.wCampo(tcDe2, '#15', PrefixTag('percentualJuroMoraTitulo'), 01, 05, 1, ValorMoraJuros, DSC_VALOR_MORA_JUROS)
    else
      Gerador.wCampo(tcDe2, '#16', PrefixTag('valorJuroMoraTitulo'     ), 01, 19, 1, ValorMoraJuros, DSC_VALOR_MORA_JUROS);

    if (PercentualMulta > 0) and (DataMulta > 0) then
    begin
        if MultaValorFixo then
        begin
          Gerador.wCampo(tcInt, '#17', PrefixTag('codigoTipoMulta'    ), 01, 01, 1, '1', DSC_CODIGO_MORA_JUROS);
          if (DataMulta > 0) then
            Gerador.wCampo(tcStr, '#18', PrefixTag('dataMultaTitulo'      ), 10, 10, 1, FormatDateTime('dd.mm.yyyy',DataMulta), DSC_DATA_MULTA);
          Gerador.wCampo(tcDe2, '#20', PrefixTag('valorMultaTitulo'   ), 01, 19, 1, PercentualMulta, DSC_PERCENTUAL_MULTA)
        end
        else
        begin
          Gerador.wCampo(tcInt, '#17', PrefixTag('codigoTipoMulta'      ), 01, 01, 1, '2', DSC_CODIGO_MORA_JUROS);
          if (DataMulta > 0) then
            Gerador.wCampo(tcStr, '#18', PrefixTag('dataMultaTitulo'      ), 10, 10, 1, FormatDateTime('dd.mm.yyyy',DataMulta), DSC_DATA_MULTA);
          Gerador.wCampo(tcDe2, '#19', PrefixTag('percentualMultaTitulo'), 01, 07, 1, PercentualMulta, DSC_PERCENTUAL_MULTA);
        end;

    end
    else
      Gerador.wCampo(tcInt, '#17', PrefixTag('codigoTipoMulta'    ), 01, 01, 1, '0', DSC_CODIGO_MORA_JUROS);

    if Aceite = atSim then
      Gerador.wCampo(tcStr, '#21', PrefixTag('codigoAceiteTitulo' ), 01, 01, 1, 'A' , DSC_ACEITE)
    else
      Gerador.wCampo(tcStr, '#21', PrefixTag('codigoAceiteTitulo' ), 01, 01, 1, 'N' , DSC_ACEITE);

    Gerador.wCampo(tcStr, '#22', PrefixTag('codigoTipoTitulo'     ), 01, 04, 1, IntToStr(codigoTipoTitulo(EspecieDoc)), DSC_TIPO_ESPECIE); //TK-2081

    Gerador.wCampo(tcStr, '#23', PrefixTag('textoDescricaoTipoTitulo'), 00, 30, 1, EspecieDoc, DSC_TIPO_ESPECIE);

    if (integer(TipoPagamento) <> 2) then
      Gerador.wCampo(tcStr, '#24', PrefixTag('indicadorPermissaoRecebimentoParcial'), 01, 01, 1, 'S', DSC_TIPO_PAGAMENTO)
    else
      Gerador.wCampo(tcStr, '#24', PrefixTag('indicadorPermissaoRecebimentoParcial'), 01, 01, 1, 'N', DSC_TIPO_PAGAMENTO);

    Gerador.wCampo(tcStr, '#25', PrefixTag('textoNumeroTituloBeneficiario'         ), 01, 15, 1, SeuNumero, DSC_NOSSO_NUMERO);
    Gerador.wCampo(tcStr, '#26', PrefixTag('textoCampoUtilizacaoBeneficiario'      ), 01, 25, 1, Boleto.Cedente.CodigoCedente, DSC_CODIGO_CEDENTE);
    Gerador.wCampo(tcStr, '#27', PrefixTag('codigoTipoContaCaucao'                 ), 01, 01, 1, '0', DSC_NOSSO_NUMERO);
    Gerador.wCampo(tcStr, '#28', PrefixTag('textoNumeroTituloCliente'              ), 01, 20, 1, '000' + ACBrUtil.Strings.PadLeft(Boleto.Cedente.Convenio, 7, '0')
                                                                                                       + ACBrUtil.Strings.PadLeft(NossoNumero, 10, '0'), DSC_NOSSO_NUMERO);

    if (Mensagem.Count > 0) then
      Gerador.wCampo(tcStr, '#29', PrefixTag('textoMensagemBloquetoOcorrencia'  ), 00, 220, 1, Mensagem.Text, DSC_MENSAGEM);

    if (Integer(Sacado.Pessoa) = 0)  then
      Gerador.wCampo(tcStr, '#30', PrefixTag('codigoTipoInscricaoPagador'       ), 01, 01, 1, '1', DSC_NOME_SACADO)
    else
      Gerador.wCampo(tcStr, '#30', PrefixTag('codigoTipoInscricaoPagador'       ), 01, 01, 1, '2', DSC_NOME_SACADO);

    Gerador.wCampo(tcStr, '#31', PrefixTag('numeroInscricaoPagador'             ), 00, 15, 1, ACBrUtil.Strings.OnlyNumber(Sacado.CNPJCPF), DSC_NOME_SACADO);
    Gerador.wCampo(tcStr, '#32', PrefixTag('nomePagador'                        ), 00, 60, 1, Sacado.NomeSacado, DSC_NOME_SACADO);
    Gerador.wCampo(tcStr, '#33', PrefixTag('textoEnderecoPagador'               ), 00, 60, 1, Sacado.Logradouro, DSC_LOGRADOURO);
    Gerador.wCampo(tcStr, '#34', PrefixTag('numeroCepPagador'                   ), 00, 08, 1, Sacado.Cep, DSC_CEP);
    Gerador.wCampo(tcStr, '#35', PrefixTag('nomeMunicipioPagador'               ), 00, 20, 1, Sacado.Cidade, DSC_CIDADE);
    Gerador.wCampo(tcStr, '#36', PrefixTag('nomeBairroPagador'                  ), 00, 20, 1, Sacado.Bairro, DSC_BAIRRO);
    Gerador.wCampo(tcStr, '#37', PrefixTag('siglaUfPagador'                     ), 00, 02, 1, Sacado.UF, DSC_UF);

    if NaoEstaVazio(trim(Sacado.Fone)) then
      Gerador.wCampo(tcStr, '#38', PrefixTag('textoNumeroTelefonePagador'         ), 00, 12, 1, Sacado.Fone, DSC_FONE);

    if NaoEstaVazio(trim(Sacado.SacadoAvalista.CNPJCPF)) then
    begin
      if (Integer(Sacado.SacadoAvalista.Pessoa) = 0)  then
        Gerador.wCampo(tcStr, '#39', PrefixTag('codigoTipoInscricaoAvalista'       ), 01, 01, 1, '1', DSC_NOME_AVALISTA)
      else
        Gerador.wCampo(tcStr, '#39', PrefixTag('codigoTipoInscricaoAvalista'       ), 01, 01, 1, '2', DSC_NOME_AVALISTA);

      Gerador.wCampo(tcStr, '#40', PrefixTag('numeroInscricaoAvalista'             ), 00, 15, 1, OnlyNumber( Sacado.SacadoAvalista.CNPJCPF ), DSC_NOME_AVALISTA);
      Gerador.wCampo(tcStr, '#41', PrefixTag('nomeAvalistaTitulo'                  ), 00, 60, 1, Sacado.SacadoAvalista.NomeAvalista, DSC_NOME_AVALISTA);
    end;

    if NaoEstaVazio( Boleto.Cedente.CedenteWS.KeyUser ) then
      lCodigoChaveUsuario:= Boleto.Cedente.CedenteWS.KeyUser
    else
      lCodigoChaveUsuario:=  C_KEYUSER;
    Gerador.wCampo(tcStr, '#42', PrefixTag('codigoChaveUsuario'       ), 01, 08, 1, lCodigoChaveUsuario, DSC_KEYUSER);
    Gerador.wCampo(tcStr, '#43', PrefixTag('codigoTipoCanalSolicitacao'       ), 01, 01, 1, C_CHANNEL, DSC_CANAL_SOLICITACAO);

  end;

end;

function TBoletoW_BancoBrasil.PrefixTag(AValue: String): String;
begin
  Result := C_PREFIX + trim(AValue);
end;

function TBoletoW_BancoBrasil.CodigoTipoTitulo(AEspecieDoc: String): Integer;
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

constructor TBoletoW_BancoBrasil.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;
  end;

end;

function TBoletoW_BancoBrasil.GerarRemessa: String;
begin
  Result:=inherited GerarRemessa;

end;

function TBoletoW_BancoBrasil.Enviar: Boolean;
begin
  Result:=inherited Enviar;

end;

end.

