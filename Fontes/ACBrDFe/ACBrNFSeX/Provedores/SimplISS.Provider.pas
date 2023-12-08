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

unit SimplISS.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceSimplISS = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderSimplISS = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

  TACBrNFSeXWebserviceSimplISS203 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSimplISS203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;
  public
    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrXmlWriter,
  ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais,
  SimplISS.GravarXml, SimplISS.LerXml;

{ TACBrNFSeProviderSimplISS }

procedure TACBrNFSeProviderSimplISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    identificador := 'id';
    UseCertificateHTTP := False;
    DetalharServico := True;

    Autenticacao.RequerLogin := True;
  end;

  SetXmlNameSpace('http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd');

  with ConfigMsgDados do
  begin
    Prefixo := 'nfse';
    PrefixoTS := 'nfse';
  end;

  SetNomeXSD('nfse_3.xsd');
end;

function TACBrNFSeProviderSimplISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SimplISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimplISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SimplISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimplISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSimplISS.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderSimplISS.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  xXml := Response.ArquivoEnvio;

  case aMetodo of
    tmRecepcionar:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:EnviarLoteRpsEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:EnviarLoteRpsEnvio>', False);

        xXml := '<sis:EnviarLoteRpsEnvio>' + xXml + '</sis:EnviarLoteRpsEnvio>';
      end;

    tmConsultarSituacao:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:ConsultarSituacaoLoteRpsEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:ConsultarSituacaoLoteRpsEnvio>', False);

        xXml := '<sis:ConsultarSituacaoLoteRpsEnvio>' + xXml + '</sis:ConsultarSituacaoLoteRpsEnvio>';
      end;

    tmConsultarLote:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:ConsultarLoteRpsEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:ConsultarLoteRpsEnvio>', False);

        xXml := '<sis:ConsultarLoteRpsEnvio>' + xXml + '</sis:ConsultarLoteRpsEnvio>';
      end;

    tmConsultarNFSePorRps:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:ConsultarNfseRpsEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:ConsultarNfseRpsEnvio>', False);

        xXml := '<sis:ConsultarNfseRpsEnvio>' + xXml + '</sis:ConsultarNfseRpsEnvio>';
      end;

    tmConsultarNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:ConsultarNfseEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:ConsultarNfseEnvio>', False);

        xXml := '<sis:ConsultarNfseEnvio>' + xXml + '</sis:ConsultarNfseEnvio>';
      end;

    tmCancelarNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfse:CancelarNfseEnvio xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">',
          '</nfse:CancelarNfseEnvio>', False);

        xXml := '<sis:CancelarNfseEnvio>' + xXml + '</sis:CancelarNfseEnvio>';
      end;
  else
    Response.ArquivoEnvio := xXml;
  end;

  Response.ArquivoEnvio := xXml;
end;

{ TACBrNFSeXWebserviceSimplISS }

function TACBrNFSeXWebserviceSimplISS.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<sis:pParam>' +
                '<sis1:P1>' + Emitente.WSUser + '</sis1:P1>' +
                '<sis1:P2>' +
                  ParseText(AnsiString(Emitente.WSSenha), False) +
                '</sis1:P2>' +
              '</sis:pParam>';
  end;
end;

function TACBrNFSeXWebserviceSimplISS.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:RecepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:RecepcionarLoteRps>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/RecepcionarLoteRps',
                     Request,
                     ['RecepcionarLoteRpsResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:ConsultarSituacaoLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:ConsultarSituacaoLoteRps>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarSituacaoLoteRps',
                     Request,
                     ['ConsultarSituacaoLoteRpsResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:ConsultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:ConsultarLoteRps>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarLoteRps',
                     Request,
                     ['ConsultarLoteRpsResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:ConsultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:ConsultarNfsePorRps>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarNfsePorRps',
                     Request,
                     ['ConsultarNfsePorRpsResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:ConsultarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:ConsultarNfse>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarNfse',
                     Request,
                     ['ConsultarNfseResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.Cancelar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<sis:CancelarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</sis:CancelarNfse>';

  Result := Executar('http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/CancelarNfse',
                     Request,
                     ['CancelarNfseResult'],
                     ['xmlns:sis="http://www.sistema.com.br/Sistema.Ws.Nfse"',
                      'xmlns:sis1="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn"',
            'xmlns:nfse="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"']);
end;

function TACBrNFSeXWebserviceSimplISS.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverPrefixosDesnecessarios(Result);
end;

{ TACBrNFSeProviderSimplISS203 }

procedure TACBrNFSeProviderSimplISS203.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ModoEnvio := meLoteAssincrono;

  ConfigGeral.ServicosDisponibilizados.EnviarLoteSincrono := False;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
  ConfigMsgDados.GerarNSLoteRps := True;
end;

function TACBrNFSeProviderSimplISS203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SimplISS203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimplISS203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SimplISS203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimplISS203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSimplISS203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderSimplISS203.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := aXml;
end;

function TACBrNFSeProviderSimplISS203.RegimeEspecialTributacaoDescricao(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  case t of
    retMicroempresaMunicipal     : Result := '1 - Microempresa municipal';
    retEstimativa                : Result := '2 - Estimativa';
    retSociedadeProfissionais    : Result := '3 - Sociedade de profissionais';
    retCooperativa               : Result := '4 - Cooperativa';
    retMicroempresarioIndividual : Result := '5 - Microempresário Individual (MEI)';
    retMicroempresarioEmpresaPP  : Result := '6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
    retTribFaturamentoVariavel   : Result := '7 - Tributação por Faturamento (Variável)';
    retFixo                      : Result := '8 - Fixo';
    retIsencao                   : Result := '9 - Isenção';
    retImune                     : Result := '10 - Imune';
    retExigibSuspensaJudicial    : Result := '11 - Exigibilidade suspensa por decisão judicial';
    retExigibSuspensaAdm         : Result := '12 - Exigibilidade suspensa por procedimento administrativo';
  else
    Result := '';
  end;
end;

function TACBrNFSeProviderSimplISS203.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  Result := EnumeradoToStr(t,
                         ['', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                          '10', '11', '12'],
                         [retNenhum, retMicroempresaMunicipal, retEstimativa,
                         retSociedadeProfissionais, retCooperativa,
                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                         retTribFaturamentoVariavel, retFixo, retIsencao,
                         retImune, retExigibSuspensaJudicial,
                         retExigibSuspensaAdm]);
end;

function TACBrNFSeProviderSimplISS203.StrToRegimeEspecialTributacao(
  out ok: boolean; const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                        ['', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                         '10', '11', '12'],
                        [retNenhum, retMicroempresaMunicipal, retEstimativa,
                         retSociedadeProfissionais, retCooperativa,
                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                         retTribFaturamentoVariavel, retFixo, retIsencao,
                         retImune, retExigibSuspensaJudicial,
                         retExigibSuspensaAdm]);
end;

{ TACBrNFSeXWebserviceSimplISS203 }

function TACBrNFSeXWebserviceSimplISS203.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseFaixaRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/ConsultarNfseFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/ConsultarNfseRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/INfseService/SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSimplISS203.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
end;

{
  Para a versão 1 desse provedor todas as tags recebem prefixo, inclusive as
  referente a montagem do lote de envio, por exemplo.
}
end.
