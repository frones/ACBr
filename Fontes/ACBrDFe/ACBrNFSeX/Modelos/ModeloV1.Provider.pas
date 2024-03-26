{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ModeloV1.Provider;
{
  Trocar todas as ocorrencias de "ModeloV1" pelo nome do provedor
}

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceModeloV1 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderModeloV1 = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrDFeException,
  ModeloV1.GravarXml, ModeloV1.LerXml;

{ TACBrNFSeProviderModeloV1 }

procedure TACBrNFSeProviderModeloV1.Configuracao;
begin
  inherited Configuracao;
  {
     Todos os parâmetros de configuração estão com os seus valores padrões.

     Se a configuração padrão atende o provedor ela pode ser excluida dessa
     procedure.

     Portanto deixe somente os parâmetros de configuração que foram alterados
     para atender o provedor.
  }

  // Inicializa os parâmetros de configuração: Geral
  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;
    NumMaxRpsGerar  := 1;
    NumMaxRpsEnviar := 50;

    TabServicosExt := False;
    Identificador := 'Id';
    QuebradeLinha := ';';

    // meLoteAssincrono, meLoteSincrono ou meUnitario
    ModoEnvio := meLoteAssincrono;

    ConsultaSitLote := True;
    ConsultaLote := True;
    ConsultaNFSe := True;
    ConsultaPorFaixa := False;
    CancPreencherMotivo := False;
    CancPreencherSerieNfse := False;
    CancPreencherCodVerificacao := False;
  end;

  // Inicializa os parâmetros de configuração: WebServices
  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
    AtribVerLote := '';
  end;

  // Define o NameSpace utilizado por todos os serviços disponibilizados pelo
  // provedor
  SetXmlNameSpace('http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd');

  // Inicializa os parâmetros de configuração: Mensagem de Dados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';

    UsarNumLoteConsLote := False;

    DadosCabecalho := GetCabecalho('');

    { caso tenha um cabeçalho fora do padrão montar ele conforme exemplo abaixo

    DadosCabecalho := '<cabecalho versao="1.00" xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
                      '<versaoDados>1.00</versaoDados>' +
                      '</cabecalho>';
    }

    // Usado para geração do Xml do Rps
    // Define o NameSpace do XML do Rps, sobrepõe a definição global: SetXmlNameSpace
    XmlRps.xmlns := '';
    XmlRps.InfElemento := 'InfRps';
    XmlRps.DocElemento := 'Rps';

    // Usado para geração do Envio do Lote em modo assíncrono
    LoteRps.xmlns := '';
    LoteRps.InfElemento := 'LoteRps';
    LoteRps.DocElemento := 'EnviarLoteRpsEnvio';

    // Usado para geração do Envio do Lote em modo Sincrono
    LoteRpsSincrono.xmlns := '';
    LoteRpsSincrono.InfElemento := 'LoteRps';
    LoteRpsSincrono.DocElemento := 'EnviarLoteRpsSincronoEnvio';

    // Usado para geração da Consulta a Situação do Lote
    ConsultarSituacao.xmlns := '';
    ConsultarSituacao.InfElemento := '';
    ConsultarSituacao.DocElemento := 'ConsultarSituacaoLoteRpsEnvio';

    // Usado para geração da Consulta do Lote
    ConsultarLote.xmlns := '';
    ConsultarLote.InfElemento := '';
    ConsultarLote.DocElemento := 'ConsultarLoteRpsEnvio';

    // Usado para geração da Consulta da NFSe por RPS
    ConsultarNFSeRps.xmlns := '';
    ConsultarNFSeRps.InfElemento := '';
    ConsultarNFSeRps.DocElemento := 'ConsultarNfseRpsEnvio';

    // Usado para geração da Consulta da NFSe
    ConsultarNFSe.xmlns := '';
    ConsultarNFSe.InfElemento := '';
    ConsultarNFSe.DocElemento := 'ConsultarNfseEnvio';

    // Usado para geração do Cancelamento
    CancelarNFSe.xmlns := '';
    CancelarNFSe.InfElemento := 'InfPedidoCancelamento';
    CancelarNFSe.DocElemento := 'Pedido';

    // Usado para geração do Gerar
    GerarNFSe.xmlns := '';
    GerarNFSe.InfElemento := '';
    GerarNFSe.DocElemento := 'GerarNfseEnvio';

    // Usado para geração do Substituir
    SubstituirNFSe.xmlns := '';
    SubstituirNFSe.InfElemento := 'SubstituicaoNfse';
    SubstituirNFSe.DocElemento := 'SubstituirNfseEnvio';

    // Usado para geração da Abertura de Sessão
    AbrirSessao.xmlns := '';
    AbrirSessao.InfElemento := '';
    AbrirSessao.DocElemento := '';

    // Usado para geração do Fechamento de Sessão
    FecharSessao.xmlns := '';
    FecharSessao.InfElemento := '';
    FecharSessao.DocElemento := '';
  end;

  // Inicializa os parâmetros de configuração: Assinar
  with ConfigAssinar do
  begin
    Rps               := False;
    LoteRps           := False;
    ConsultarSituacao := False;
    ConsultarLote     := False;
    ConsultarNFSeRps  := False;
    ConsultarNFSe     := False;
    CancelarNFSe      := False;
    RpsGerarNFSe      := False;
    LoteGerarNFSe     := False;
    RpsSubstituirNFSe := False;
    SubstituirNFSe    := False;
    AbrirSessao       := False;
    FecharSessao      := False;

    IncluirURI := True;

    AssinaturaAdicional := False;
  end;

  // Define o nome do arquivo XSD utilizado para todos os serviços disponibilizados
  // pelo provedor
  SetNomeXSD('nfse.xsd');

  // Define o nome do arquivo XSD para cada serviços disponibilizado pelo
  // provedor
  with ConfigSchemas do
  begin
    Recepcionar := 'nfse.xsd';
    ConsultarSituacao := 'nfse.xsd';
    ConsultarLote := 'nfse.xsd';
    ConsultarNFSeRps := 'nfse.xsd';
    ConsultarNFSe := 'nfse.xsd';
    ConsultarNFSePorFaixa := 'nfse.xsd';
    ConsultarNFSeServicoPrestado := 'nfse.xsd';
    ConsultarNFSeServicoTomado := 'nfse.xsd';
    CancelarNFSe := 'nfse.xsd';
    GerarNFSe := 'nfse.xsd';
    RecepcionarSincrono := 'nfse.xsd';
    SubstituirNFSe := 'nfse.xsd';
    AbrirSessao := 'nfse.xsd';
    FecharSessao := 'nfse.xsd';
    Teste := 'nfse.xsd';
    Validar := True;
  end;
end;

function TACBrNFSeProviderModeloV1.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ModeloV1.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderModeloV1.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ModeloV1.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderModeloV1.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceModeloV1.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceModeloV1 }

function TACBrNFSeXWebserviceModeloV1.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:RecepcionarLoteRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarLoteRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarSituacaoLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarSituacaoLoteRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarSituacaoLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarNfsePorRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:CancelarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceModeloV1.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
end;

end.
