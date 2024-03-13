{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit Ginfes.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceGinfes = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property NameSpace: string read GetNameSpace;
  end;

  TACBrNFSeProviderGinfes = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function DefinirIDCancelamento(const CNPJ: string; const InscMunic: string;
                                   const NumNfse: string): string; override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;

  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrXmlDocument,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Ginfes.GravarXml, Ginfes.LerXml;

{ TACBrNFSeProviderGinfes }

procedure TACBrNFSeProviderGinfes.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.QuebradeLinha := '\n';

  with ConfigMsgDados do
  begin
    Prefixo := 'ns3';
    PrefixoTS := 'ns4';

    DadosCabecalho := '<ns2:cabecalho versao="3" xmlns:ns2="http://www.ginfes.com.br/cabecalho_v03.xsd">' +
                      '<versaoDados>3</versaoDados>' +
                      '</ns2:cabecalho>';

    XmlRps.xmlns := 'http://www.ginfes.com.br/tipos_v03.xsd';

    LoteRps.xmlns := 'http://www.ginfes.com.br/servico_enviar_lote_rps_envio_v03.xsd';

    ConsultarSituacao.xmlns := 'http://www.ginfes.com.br/servico_consultar_situacao_lote_rps_envio_v03.xsd';

    ConsultarLote.xmlns := 'http://www.ginfes.com.br/servico_consultar_lote_rps_envio_v03.xsd';

    ConsultarNFSeRps.xmlns := 'http://www.ginfes.com.br/servico_consultar_nfse_rps_envio_v03.xsd';

    ConsultarNFSe.xmlns := 'http://www.ginfes.com.br/servico_consultar_nfse_envio_v03.xsd';

    CancelarNFSe.xmlns := 'http://www.ginfes.com.br/servico_cancelar_nfse_envio_v03.xsd';
  end;

  with ConfigAssinar do
  begin
    LoteRps           := True;
    ConsultarSituacao := True;
    ConsultarLote     := True;
    ConsultarNFSeRps  := True;
    ConsultarNFSe     := True;
    CancelarNFSe      := True;
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio_v03.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio_v03.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio_v03.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio_v03.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio_v03.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio_v03.xsd';

    Validar := False;
  end;
end;

function TACBrNFSeProviderGinfes.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Ginfes.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGinfes.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Ginfes.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGinfes.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderGinfes.DefinirIDCancelamento(const CNPJ, InscMunic,
  NumNfse: string): string;
begin
  Result := '';
end;

procedure TACBrNFSeProviderGinfes.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
begin
  with ConfigMsgDados do
  begin
    Prefixo := '';
    PrefixoTS := '';
  end;

  inherited PrepararCancelaNFSe(Response);

  with ConfigMsgDados do
  begin
    Prefixo := 'ns3';
    PrefixoTS := 'ns4';
  end;
end;

procedure TACBrNFSeProviderGinfes.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  CodCanc: Integer;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;
  CodCanc := StrToIntDef(InfoCanc.CodCancelamento, 1);

  with Params do
  begin
    NameSpace := ' xmlns="http://www.ginfes.com.br/servico_cancelar_nfse_envio_v03.xsd" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

    Response.ArquivoEnvio := '<CancelarNfseEnvio' + NameSpace + '>' +
                               '<Pedido xmlns="">' +
                                 '<InfPedidoCancelamento xmlns="http://www.ginfes.com.br/tipos_v03.xsd">' +
                                   '<IdentificacaoNfse>' +
                                     '<Numero>' +
                                       InfoCanc.NumeroNFSe +
                                     '</Numero>' +
                                     '<Cnpj>' +
                                       OnlyNumber(Emitente.CNPJ) +
                                     '</Cnpj>' +
                                     GetInscMunic(Emitente.InscMun, Prefixo2) +
                                    '<CodigoMunicipio>' +
                                      ConfigGeral.CodIBGE +
                                    '</CodigoMunicipio>' +
                                   '</IdentificacaoNfse>' +
                                   '<CodigoCancelamento>' +
                                     FormatFloat('0000', CodCanc) +
                                   '</CodigoCancelamento>' +
                                   Motivo +
                                 '</InfPedidoCancelamento>' +
                               '</Pedido>' +
                             '</CancelarNfseEnvio>';
  end;
end;

{ TACBrNFSeXWebserviceGinfes }

function TACBrNFSeXWebserviceGinfes.GetNameSpace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := 'http://homologacao.ginfes.com.br'
  else
    Result := 'http://producao.ginfes.com.br';

  Result := ' xmlns:ns1="' + Result + '"';
end;

function TACBrNFSeXWebserviceGinfes.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:RecepcionarLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:RecepcionarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarSituacaoLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarSituacaoLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfsePorRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarNfsePorRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfseV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarNfseV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceGinfes.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:CancelarNfseV3>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:CancelarNfseV3>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceGinfes.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);

  if Pos('ns1:ConsultarNfsePorRpsV3Response', Result) > 0 then
    Result := StringReplace(Result, 'ns3:ConsultarNfseResposta', 'ns3:ConsultarNfseRpsResposta', [rfReplaceAll]);

  Result := RemoverPrefixosDesnecessarios(Result);
end;

end.
