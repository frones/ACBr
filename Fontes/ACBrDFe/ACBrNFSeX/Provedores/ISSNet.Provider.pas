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

{
  Só funcionou usando o HttpWinNet
}

{$I ACBr.inc}

unit ISSNet.Provider;

interface

uses
  SysUtils, Classes,
  ACBrConsts, ACBrXmlBase, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrXmlDocument,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSNet = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderISSNet = class (TACBrNFSeProviderABRASFv1)
  protected
    FpNameSpaceCanc: string;

    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
  end;

  TACBrNFSeXWebserviceISSNet204 = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function ConsultarLinkNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderISSNet204 = class (TACBrNFSeProviderABRASFv2)
  public
    function NaturezaOperacaoDescricao(const t: TnfseNaturezaOperacao): string; override;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); override;

    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); override;

    procedure GerarMsgDadosConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;

    procedure GerarMsgDadosConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;

    procedure GerarMsgDadosConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;

    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
    procedure GerarMsgDadosConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSNet.GravarXml, ISSNet.LerXml, StrUtils, DateUtils;

{ TACBrNFSeProviderISSNet }

procedure TACBrNFSeProviderISSNet.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    {
    with TACBrNFSeX(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        CodIBGE := IntToStr(Configuracoes.Geral.CodigoMunicipio)
      else
        CodIBGE := '999';
    end;
    }
  end;

  with ConfigMsgDados do
  begin
    PrefixoTS := 'tc';

    XmlRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd';

    LoteRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_enviar_lote_rps_envio.xsd';

    ConsultarSituacao.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_situacao_lote_rps_envio.xsd';

    ConsultarLote.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_lote_rps_envio.xsd';

    ConsultarNFSeRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_rps_envio.xsd';

    ConsultarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_envio.xsd';

    CancelarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd';

    ConsultarLinkNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_url_visualizacao_nfse_serie_envio.xsd';
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio.xsd';
    ConsultarLinkNFSe := 'servico_consultar_url_visualizacao_nfse_serie_envio.xsd';
  end;

  FpNameSpaceCanc := ' xmlns:ts="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_simples.xsd"' +
                     ' xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"' +
                     ' xmlns:p1="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd"';
end;

function TACBrNFSeProviderISSNet.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSNet.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderISSNet.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    NameSpace := FpNameSpaceCanc;

    Response.ArquivoEnvio := '<Pedido' + NameSpace + '>' +
                           '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + '>' +
                             '<' + Prefixo2 + 'IdentificacaoNfse>' +
                               '<' + Prefixo2 + 'Numero>' +
                                 InfoCanc.NumeroNFSe +
                               '</' + Prefixo2 + 'Numero>' +
                               '<' + Prefixo2 + 'Cnpj>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</' + Prefixo2 + 'Cnpj>' +
                               GetInscMunic(Emitente.InscMun, Prefixo2) +
                               '<' + Prefixo2 + 'CodigoMunicipio>' +
                                  ConfigGeral.CodIBGE +
                               '</' + Prefixo2 + 'CodigoMunicipio>' +
                             '</' + Prefixo2 + 'IdentificacaoNfse>' +
                             '<' + Prefixo2 + 'CodigoCancelamento>' +
                                InfoCanc.CodCancelamento +
                             '</' + Prefixo2 + 'CodigoCancelamento>' +
                             Motivo +
                           '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                         '</Pedido>';
  end;
end;

procedure TACBrNFSeProviderISSNet.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
  i, j: Integer;
begin
  if aMetodo <> tmCancelarNFSe then
  begin
    xXml := Response.ArquivoEnvio;

    i := Pos('<tc:Cnpj>', xXml) -1;
    j := Pos('<tc:InscricaoMunicipal>', xXml);

    xXml := Copy(xXml, 1, i) +
            '<tc:CpfCnpj>' + Copy(xXml, i+1, j-i-1) + '</tc:CpfCnpj>' +
            Copy(xXml, j, Length(xXml));

    Response.ArquivoEnvio := xXml;

    inherited ValidarSchema(Response, aMetodo);
  end
  else
  begin
    xXml := Response.ArquivoEnvio;
    xXml := StringReplace(xXml, FpNameSpaceCanc, '', []);
    xXml := '<p1:CancelarNfseEnvio' + FpNameSpaceCanc + '>' +
              xXml +
            '</p1:CancelarNfseEnvio>';

    Response.ArquivoEnvio := xXml;
  end;
end;

{ TACBrNFSeXWebserviceISSNet }

function TACBrNFSeXWebserviceISSNet.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/RecepcionarLoteRps',
                     Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarLoteRps',
                     Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultaSituacaoLoteRPS xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultaSituacaoLoteRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultaSituacaoLoteRPS',
                     Request,
                     ['ConsultaSituacaoLoteRPSResult', 'ConsultarSituacaoLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNFSePorRPS xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarNFSePorRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNFSePorRPS',
                     Request,
                     ['ConsultarNFSePorRPSResult', 'ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfse xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNfse',
                     Request,
                     ['ConsultarNfseResult', 'ConsultarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</CancelarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/CancelarNfse',
                     Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

{ TACBrNFSeXWebserviceISSNet204 }

function TACBrNFSeXWebserviceISSNet204.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', Request,
//                     ['outputXML', 'EnviarLoteRpsResposta'],
                     ['EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincrono>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', Request,
//                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', Request,
//                     ['outputXML', 'GerarNfseResposta'],
                     ['GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeProviderISSNet204.NaturezaOperacaoDescricao(
  const t: TnfseNaturezaOperacao): string;
begin
  case t of
    no1 : Result := '1 - Exigível';
    no2 : Result := '2 - Não Incidência';
    no4 : Result := '4 - Exportação';
    no5 : Result := '5 - Imunidade';
    no6 : Result := '6 - Exigibilidade Suspensa por Decisão Judicial';
    no7 : Result := '7 - Exigibilidade Suspensa por Processo Administrativo';
  else
    Result := inherited NaturezaOperacaoDescricao(t);
  end;
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', Request,
//                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixa>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixa>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorFaixa', Request,
//                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     ['ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', Request,
//                     ['outputXML', 'ConsultarNfsePorRpsResposta'],
                     ['ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', Request,
//                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', Request,
//                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.ConsultarLinkNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarUrlNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarUrlNfse>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarUrlNfse', Request,
                     ['ConsultarUrlNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.Cancelar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', Request,
//                     ['outputXML', 'CancelarNfseResposta'],
                     ['CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfse>';

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', Request,
//                     ['outputXML', 'SubstituirNfseResposta'],
                     ['SubstituirNfseResult'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSNet204.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

{ TACBrNFSeProviderISSNet204 }

procedure TACBrNFSeProviderISSNet204.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ConsultaPorFaixaPreencherNumNfseFinal := True;

    ServicosDisponibilizados.ConsultarLinkNfse := True;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    ConsultarLinkNFSe := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    SubstituirNFSe := True;

    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '1.00';
  end;

  with ConfigMsgDados do
  begin
    DadosCabecalho := GetCabecalho('');
    GerarPrestadorLoteRps := True;
  end;
end;

function TACBrNFSeProviderISSNet204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSNet204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSNet204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSNet204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador, NumeroLote: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador := '<Prestador>' +
                   '<CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ) +
                   '</CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun) +
                 '</Prestador>' +
                 '<Protocolo>' +
                   Response.Protocolo +
                 '</Protocolo>';

    if ConfigMsgDados.UsarNumLoteConsLote then
      NumeroLote := '<NumeroLote>' +
                      Response.NumeroLote +
                    '</NumeroLote>';

    Response.ArquivoEnvio := '<' + TagEnvio + NameSpace + '>' +
                                 Prestador +
                                 NumeroLote +
                             '</' + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<Prestador>' +
                  '<CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ) +
                  '</CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun) +
                '</Prestador>';

    Response.ArquivoEnvio := '<' + TagEnvio + NameSpace + '>' +
                               '<Pedido>' +
                                 '<IdentificacaoRps>' +
                                   '<Numero>' +
                                     Response.NumeroRps +
                                   '</Numero>' +
                                   '<Serie>' +
                                     Response.SerieRps +
                                   '</Serie>' +
                                   '<Tipo>' +
                                     Response.TipoRps +
                                   '</Tipo>' +
                                 '</IdentificacaoRps>' +
                                 Prestador +
                               '</Pedido>' +
                             '</' + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<Prestador>' +
                  '<CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ) +
                  '</CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun) +
                '</Prestador>';

    Response.ArquivoEnvio := '<ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 Prestador +
                                 Xml +
                                 '<Pagina>' +
                                    IntToStr(Response.InfConsultaNFSe.Pagina) +
                                 '</Pagina>' +
                               '</Pedido>' +
                             '</ConsultarNfseFaixaEnvio>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<Prestador>' +
                  '<CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ) +
                  '</CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun) +
                '</Prestador>';

    Response.ArquivoEnvio := '<ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 Prestador +
                                 Xml +
                                 '<Pagina>' +
                                    IntToStr(Response.InfConsultaNFSe.Pagina) +
                                 '</Pagina>' +
                               '</Pedido>' +
                             '</ConsultarNfseServicoPrestadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Consulente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Consulente :='<Consulente>' +
                   '<CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ) +
                   '</CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun) +
                 '</Consulente>';

    Response.ArquivoEnvio := '<ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 Consulente +
                                 Xml +
                                 '<Pagina>' +
                                    IntToStr(Response.InfConsultaNFSe.Pagina) +
                                 '</Pagina>' +
                               '</Pedido>' +
                             '</ConsultarNfseServicoTomadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  xCodMun: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with TACBrNFSeX(FAOwner) do
  begin
    if Configuracoes.WebServices.AmbienteCodigo = 1 then
      xCodMun := IntToStr(Configuracoes.Geral.CodigoMunicipio)
    else
      xCodMun := '5002704';
  end;

  with Params do
  begin
    Response.ArquivoEnvio := '<CancelarNfseEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 '<InfPedidoCancelamento' + IdAttr + NameSpace2 + '>' +
                                   '<IdentificacaoNfse>' +
                                     '<Numero>' +
                                        InfoCanc.NumeroNFSe +
                                     '</Numero>' +
                                     Serie +
                                     '<CpfCnpj>' +
                                       GetCpfCnpj(Emitente.CNPJ) +
                                     '</CpfCnpj>' +
                                     GetInscMunic(Emitente.InscMun) +
                                     '<CodigoMunicipio>' +
                                       xCodMun +
                                     '</CodigoMunicipio>' +
                                     CodigoVerificacao +
                                   '</IdentificacaoNfse>' +
                                   '<CodigoCancelamento>' +
                                      InfoCanc.CodCancelamento +
                                   '</CodigoCancelamento>' +
                                   Motivo +
                                 '</InfPedidoCancelamento>' +
                               '</Pedido>' +
                             '</CancelarNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  aParams: TNFSeParamsResponse;
  NameSpace, TagEnvio, Prefixo: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.CNPJ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod130;
    AErro.Descricao := ACBrStr(Desc130);
    Exit;
  end;

  if EstaVazio(Emitente.InscMun) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod129;
    AErro.Descricao := ACBrStr(Desc129);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaLinkNFSe.NumeroNFSe) then
  begin
    // Obrigatoriamente NumeroNFSe ou 1 dos grupos, somente 1, deve ser enviado
    if ((Response.InfConsultaLinkNFSe.NumeroRps = 0) and
        (Response.InfConsultaLinkNFSe.Competencia = 0) and
        (Response.InfConsultaLinkNFSe.DtEmissao = 0)) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod108;
      AErro.Descricao := ACBrStr(Desc108);
      Exit;
    end

    else if Response.InfConsultaLinkNFSe.NumeroRps <> 0 then
    begin
      if EstaVazio(Response.InfConsultaLinkNFSe.SerieRps) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod103;
        AErro.Descricao := ACBrStr(Desc103);
        Exit;
      end;

      if EstaVazio(Response.InfConsultaLinkNFSe.TipoRps) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod104;
        AErro.Descricao := ACBrStr(Desc104);
        Exit;
      end;
    end;
  end;

  if Response.InfConsultaLinkNFSe.Pagina = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod132;
    AErro.Descricao := ACBrStr(Desc132);
    Exit;
  end;

  if EstaVazio(ConfigMsgDados.ConsultarLinkNFSe.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarLinkNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarLinkNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  TagEnvio := ConfigMsgDados.ConsultarLinkNFSe.DocElemento;

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := TagEnvio;
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := '';
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaLinkNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderISSNet204.GerarMsgDadosConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  DataFinal: TDateTime;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  DataFinal := EndOfTheMonth(Response.InfConsultaLinkNFSe.Competencia);
  if DataFinal <> 0 then
  begin
    if DataFinal > Date then
      DataFinal := Date;
  end;

  with Params do
  begin
    Response.ArquivoEnvio := '<ConsultarUrlNfseEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 '<Prestador>' +
                                   '<CpfCnpj>' +
                                     GetCpfCnpj(Emitente.CNPJ) +
                                   '</CpfCnpj>' +
                                   GetInscMunic(Emitente.InscMun) +
                                 '</Prestador>' +
                               IfThen(not EstaVazio(Response.InfConsultaLinkNFSe.NumeroNFSe),
                                    '<NumeroNfse>' + Response.InfConsultaLinkNFSe.NumeroNFSe + '</NumeroNfse>',
                               IfThen(Response.InfConsultaLinkNFSe.NumeroRps <> 0,
                                 '<IdentificacaoRps>' +
                                   '<Numero>' + IntToStr(Response.InfConsultaLinkNFSe.NumeroRps) + '</Numero>' +
                                   '<Serie>' + Response.InfConsultaLinkNFSe.SerieRps + '</Serie>' +
                                   '<Tipo>' + Response.InfConsultaLinkNFSe.TipoRps + '</Tipo>' +
                                 '</IdentificacaoRps>',
                               IfThen(Response.InfConsultaLinkNFSe.DtEmissao <> 0,
                                 '<PeriodoEmissao>' +
                                   '<DataInicial>' + FormatDateTime('yyyy-mm-dd', Response.InfConsultaLinkNFSe.DtEmissao) + '</DataInicial>' +
                                   '<DataFinal>' + FormatDateTime('yyyy-mm-dd', Response.InfConsultaLinkNFSe.DtEmissao) + '</DataFinal>' +
                                 '</PeriodoEmissao>',
                                 IfThen(Response.InfConsultaLinkNFSe.Competencia <> 0,
                                   '<PeriodoCompetencia>' +
                                     '<DataInicial>' + FormatDateTime('yyyy-mm-dd', StartOfTheMonth(Response.InfConsultaLinkNFSe.Competencia)) + '</DataInicial>' +
                                     '<DataFinal>' + FormatDateTime('yyyy-mm-dd', DataFinal) + '</DataFinal>' +
                                   '</PeriodoCompetencia>', '')))) +
                                 '<Pagina>' + IntToStr(Response.InfConsultaLinkNFSe.Pagina) + '</Pagina>' +
                               '</Pedido>' +
                             '</ConsultarUrlNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderISSNet204.TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AResumo: TNFSeResumoCollectionItem;
  I: Integer;
begin
  Document := TACBrXmlDocument.Create;
  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;
      if ANode <> nil then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('ListaLinks');

        Response.Sucesso := Assigned(AuxNode);
      end
      else
        AuxNode := nil;

      if not Response.Sucesso then
        ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno')
      else
      begin
        if not Assigned(AuxNode) then Exit;

        ANodeArray := AuxNode.Childrens.FindAllAnyNs('Links');

        if not Assigned(ANodeArray) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');
          if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

          AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

          AResumo.NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
          AResumo.SerieRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);

          AResumo.Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('UrlVisualizacaoNfse'), tcStr);

          // Grava o primeiro retorno no Response
          if Response.Link = '' then
          begin
            Response.NumeroNota := AResumo.NumeroNota;
            Response.NumeroRps := AResumo.NumeroRps;
            Response.SerieRps := AResumo.SerieRps;
            Response.Link := AResumo.Link;
          end;
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.
