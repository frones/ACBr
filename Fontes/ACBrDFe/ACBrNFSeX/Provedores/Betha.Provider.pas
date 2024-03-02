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

unit Betha.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceBetha = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderBetha = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure AssinarCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
  public
    function CondicaoPagToStr(const t: TnfseCondicaoPagamento): string; override;
    function StrToCondicaoPag(out ok: boolean; const s: string): TnfseCondicaoPagamento; override;
  end;

  TACBrNFSeXWebserviceBetha202 = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
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

  TACBrNFSeProviderBetha202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function DefinirIDLote(const ID: string): string; override;
  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, Betha.GravarXml, Betha.LerXml;

{ TACBrNFSeXWebserviceBetha }

function TACBrNFSeXWebserviceBetha.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := StringReplace(Result, '&amp;', '\s\n', [rfReplaceAll]);
  Result := ParseText(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

function TACBrNFSeXWebserviceBetha.Recepcionar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceBetha.ConsultarLote(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceBetha.ConsultarSituacao(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['ConsultarSituacaoLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceBetha.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceBetha.ConsultarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['ConsultarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceBetha.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['CancelarNfseResposta'], []);
end;

{ TACBrNFSeProviderBetha }

procedure TACBrNFSeProviderBetha.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.DetalharServico := True;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
  end;

  SetXmlNameSpace('http://www.betha.com.br/e-nota-contribuinte-ws');

  with ConfigMsgDados do
  begin
    ConsultarNFSeRps.DocElemento := 'ConsultarNfsePorRpsEnvio';
    XmlRps.xmlns := '';
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio_v01.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio_v01.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio_v01.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio_v01.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio_v01.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio_v01.xsd';
  end;
end;

function TACBrNFSeProviderBetha.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Betha.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBetha.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Betha.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBetha.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceBetha.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderBetha.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  aXml: string;
begin
  ConfigMsgDados.Prefixo := '';

  inherited PrepararEmitir(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<EnviarLoteRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</EnviarLoteRpsEnvio>', False);

  Response.ArquivoEnvio := '<ns3:EnviarLoteRpsEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         aXml +
                       '</ns3:EnviarLoteRpsEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

procedure TACBrNFSeProviderBetha.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  aXml: string;
begin
  ConfigMsgDados.Prefixo := '';

  inherited PrepararConsultaSituacao(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<ConsultarSituacaoLoteRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</ConsultarSituacaoLoteRpsEnvio>', False);

  Response.ArquivoEnvio := '<ns3:ConsultarSituacaoLoteRpsEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         aXml +
                       '</ns3:ConsultarSituacaoLoteRpsEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

procedure TACBrNFSeProviderBetha.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  aXml: string;
begin
  ConfigMsgDados.Prefixo := '';

  inherited PrepararConsultaLoteRps(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<ConsultarLoteRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</ConsultarLoteRpsEnvio>', False);

  Response.ArquivoEnvio := '<ns3:ConsultarLoteRpsEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         aXml +
                       '</ns3:ConsultarLoteRpsEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

procedure TACBrNFSeProviderBetha.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  aXml: string;
begin
  ConfigMsgDados.Prefixo := '';

  inherited PrepararConsultaNFSeporRps(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<ConsultarNfsePorRpsEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</ConsultarNfsePorRpsEnvio>', False);

  Response.ArquivoEnvio := '<ns3:ConsultarNfsePorRpsEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         aXml +
                       '</ns3:ConsultarNfsePorRpsEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

procedure TACBrNFSeProviderBetha.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  aXml: string;
begin
  ConfigMsgDados.Prefixo := '';

  inherited PrepararConsultaNFSe(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<ConsultarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</ConsultarNfseEnvio>', False);

  Response.ArquivoEnvio := '<ns3:ConsultarNfseEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         aXml +
                       '</ns3:ConsultarNfseEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

procedure TACBrNFSeProviderBetha.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  aXml: string;
begin
  inherited PrepararCancelaNFSe(Response);

  aXml := RetornarConteudoEntre(Response.ArquivoEnvio,
   '<CancelarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">',
   '</CancelarNfseEnvio>', False);

  Response.ArquivoEnvio := aXml;
end;

procedure TACBrNFSeProviderBetha.AssinarCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
begin
  inherited AssinarCancelaNFSe(Response);

  Response.ArquivoEnvio := '<ns3:CancelarNfseEnvio xmlns:ns3="http://www.betha.com.br/e-nota-contribuinte-ws">' +
                         Response.ArquivoEnvio +
                       '</ns3:CancelarNfseEnvio>';

  ConfigMsgDados.Prefixo := 'ns3';
end;

function TACBrNFSeProviderBetha.CondicaoPagToStr(
  const t: TnfseCondicaoPagamento): string;
begin
  Result := EnumeradoToStr(t,
                           ['A_VISTA', 'NA_APRESENTACAO', 'A_PRAZO', 'CARTAO_DEBITO',
                            'CARTAO_CREDITO'],
                           [cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoDebito,
                            cpCartaoCredito]);
end;

function TACBrNFSeProviderBetha.StrToCondicaoPag(out ok: boolean;
  const s: string): TnfseCondicaoPagamento;
begin
  Result := StrToEnumerado(ok, s,
                           ['A_VISTA', 'NA_APRESENTACAO', 'A_PRAZO', 'CARTAO_DEBITO',
                            'CARTAO_CREDITO'],
                           [cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoDebito,
                            cpCartaoCredito])
end;

{ TACBrNFSeProviderBetha202 }

procedure TACBrNFSeProviderBetha202.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  SetXmlNameSpace('http://www.betha.com.br/e-nota-contribuinte-ws');

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v202.xsd');
end;

function TACBrNFSeProviderBetha202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Betha202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBetha202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Betha202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBetha202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceBetha202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderBetha202.DefinirIDLote(const ID: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="lote' + ID + '"';
end;

{ TACBrNFSeXWebserviceBetha202 }

function TACBrNFSeXWebserviceBetha202.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:RecepcionarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:RecepcionarLoteRps>';

  Result := Executar('RecepcionarLoteRpsEnvio', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:RecepcionarLoteRpsSincrono>';

  Result := Executar('RecepcionarLoteRpsSincronoEnvio', Request,
                     ['return', 'EnviarLoteRpsSincronoResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:GerarNfse>';

  Result := Executar('GerarNfseEnvio', Request,
                     ['return', 'GerarNfseResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarLoteRps>';

  Result := Executar('ConsultarLoteRpsEnvio', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfseFaixa>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfseFaixa>';

  Result := Executar('ConsultarNfseFaixaEnvio', Request,
                     ['return', 'ConsultarNfseFaixaResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfsePorRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfsePorRps>';

  Result := Executar('ConsultarNfseRpsEnvio', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfseServicoPrestado>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfseServicoPrestado>';

  Result := Executar('ConsultarNfseServicoPrestadoEnvio', Request,
                     ['return', 'ConsultarNfseServicoPrestadoResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfseServicoTomado>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfseServicoTomado>';

  Result := Executar('ConsultarNfseServicoTomadoEnvio', Request,
                     ['return', 'ConsultarNfseServicoTomadoResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:CancelarNfse>';

  Result := Executar('CancelarNfseEnvio', Request,
                     ['return', 'CancelarNfseResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:SubstituirNfse>';

  {
    O WebService gera a tag com a grafia errada
    <SubstutuirNfseResposta> em vez de <SubstituirNfseResposta>
  }
  Result := Executar('SubstituirNfseEnvio', Request,
                     ['return', 'SubstutuirNfseResposta'],
                ['xmlns:tns="http://www.betha.com.br/e-nota-contribuinte-ws"']);
end;

function TACBrNFSeXWebserviceBetha202.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := StringReplace(Result, '&amp;', '\s\n', [rfReplaceAll]);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
end;

end.
