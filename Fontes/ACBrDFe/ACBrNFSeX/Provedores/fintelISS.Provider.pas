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

unit fintelISS.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebservicefintelISS200 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderfintelISS200 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  public
    function GetSchemaPath: string; override;
  end;

  TACBrNFSeXWebservicefintelISS202 = class(TACBrNFSeXWebservicefintelISS200)
  public

  end;

  TACBrNFSeProviderfintelISS202 = class (TACBrNFSeProviderfintelISS200)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
  end;

  TACBrNFSeXWebservicefintelISS204 = class(TACBrNFSeXWebservicefintelISS202)
  public
    // Métodos testados com sucesso
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    // implementado mas nao testado
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;
  end;

  TACBrNFSeProviderfintelISS204 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  public
    function GetSchemaPath: string; override;
  end;

implementation

uses
  ACBrDFe.Conversao,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, fintelISS.GravarXml, fintelISS.LerXml;

{ TACBrNFSeProviderfintelISS200 }

procedure TACBrNFSeProviderfintelISS200.Configuracao;
begin
  inherited Configuracao;

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
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace('http://www.abrasf.org.br/nfse.xsd');

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfseV202.xsd');
end;

function TACBrNFSeProviderfintelISS200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_fintelISS200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_fintelISS200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebservicefintelISS200.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderfintelISS200.GetSchemaPath: string;
begin
  Result := inherited GetSchemaPath;

  Result := Result + ConfigGeral.CodIBGE + '\';
end;

{ TACBrNFSeXWebservicefintelISS200 }

function TACBrNFSeXWebservicefintelISS200.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:RecepcionarLoteRps>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:RecepcionarLoteRps>';

  Result := Executar('http://www.fintel.com.br/WebService/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:RecepcionarLoteRpsSincrono>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:RecepcionarLoteRpsSincrono>';

  Result := Executar('http://www.fintel.com.br/WebService/RecepcionarLoteRpsSincrono', Request,
                     ['RecepcionarLoteRpsSincronoResult', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:GerarNfse>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:GerarNfse>';

  Result := Executar('http://www.fintel.com.br/WebService/GerarNfse', Request,
                     ['GerarNfseResult', 'GerarNfseResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:ConsultarLoteRps>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:ConsultarLoteRps>';

  Result := Executar('http://www.fintel.com.br/WebService/ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:ConsultarNfseFaixa>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:ConsultarNfseFaixa>';

  Result := Executar('http://www.fintel.com.br/WebService/ConsultarNfseFaixa', Request,
                     ['ConsultarNfseFaixaResult', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:ConsultarNfsePorRps>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:ConsultarNfsePorRps>';

  Result := Executar('http://www.fintel.com.br/WebService/ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResult', 'ConsultarNfseRpsResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:ConsultarNfseServicoPrestado>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:ConsultarNfseServicoPrestado>';

  Result := Executar('http://www.fintel.com.br/WebService/ConsultarNfseServicoPrestado', Request,
                     ['ConsultarNfseServicoPrestadoResult', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:ConsultarNfseServicoTomado>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:ConsultarNfseServicoTomado>';

  Result := Executar('http://www.fintel.com.br/WebService/ConsultarNfseServicoTomado', Request,
                     ['ConsultarNfseServicoTomadoResult', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:CancelarNfse>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:CancelarNfse>';

  Result := Executar('http://www.fintel.com.br/WebService/CancelarNfse', Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:SubstituirNfse>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:SubstituirNfse>';

  Result := Executar('http://www.fintel.com.br/WebService/SubstituirNfse', Request,
                     ['SubstituirNfseResult', 'SubstituirNfseResposta'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS200.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := StringReplace(Result, ' xsi:nil="true"', '', [rfReplaceAll]);
end;

{ TACBrNFSeProviderfintelISS202 }

procedure TACBrNFSeProviderfintelISS202.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.DetalharServico := True;

  ConfigGeral.Particularidades.PermiteMaisDeUmServico := True;
  ConfigGeral.Particularidades.PermiteTagOutrasInformacoes := True;

  with ConfigAssinar do
  begin
    Rps := False;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
    AtribVerLote := '';
  end;

  with ConfigMsgDados do
  begin
    XmlRps.xmlns := ConfigWebServices.Producao.XMLNameSpace;

    SetXmlNameSpace(XmlRps.xmlns);

    DadosCabecalho := GetCabecalho(XmlRps.xmlns);
  end;

  SetNomeXSD('nfseV202.xsd');
end;

function TACBrNFSeProviderfintelISS202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_fintelISS202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_fintelISS202.Create(Self);
  Result.NFSe := ANFSe;
end;

{ TACBrNFSeProviderfintelISS204 }

procedure TACBrNFSeProviderfintelISS204.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.DetalharServico := True;

  ConfigGeral.Particularidades.PermiteMaisDeUmServico := True;
  ConfigGeral.Particularidades.PermiteTagOutrasInformacoes := True;

  with ConfigAssinar do
  begin
    Rps := False;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace('http://www.abrasf.org.br/nfse.xsd');
  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
  ConfigMsgDados.GerarPrestadorLoteRps := True;
  SetNomeXSD('nfseV204.xsd');
end;

function TACBrNFSeProviderfintelISS204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_fintelISS204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_fintelISS204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebservicefintelISS204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderfintelISS204.GetSchemaPath: string;
begin
  Result := inherited GetSchemaPath;
end;

{ TACBrNFSeXWebservicefintelISS204 }

function TACBrNFSeXWebservicefintelISS204.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.ConsultarNFSePorFaixa(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseFaixa>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseFaixa>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.ConsultarNFSeServicoPrestado(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestado>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.ConsultarNFSeServicoTomado(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomado>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', Request,
                     ['outputXML', 'RecepcionarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincrono>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebservicefintelISS204.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfse>';

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

end.
