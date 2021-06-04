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
  TACBrNFSeXWebservicefintelISS = class(TACBrNFSeXWebserviceSoap11)
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

  end;

  TACBrNFSeProviderfintelISS = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, fintelISS.GravarXml, fintelISS.LerXml;

{ TACBrNFSeProviderfintelISS }

procedure TACBrNFSeProviderfintelISS.Configuracao;
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
  end;

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      if TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio <> 3136702 then
      begin
        xmlns := ConfigWebServices.Producao.XMLNameSpace;

        ConfigWebServices.AtribVerLote := '';

        SetXmlNameSpace(xmlns);
      end;
    end;

    DadosCabecalho := GetCabecalho(XmlRps.xmlns);
  end;

  SetNomeXSD('nfseV202.xsd');
end;

function TACBrNFSeProviderfintelISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_fintelISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_fintelISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderfintelISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebservicefintelISS.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebservicefintelISS }

function TACBrNFSeXWebservicefintelISS.Recepcionar(ACabecalho,
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
                     ['return', 'outputXML', 'RecepcionarLoteRpsResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.RecepcionarSincrono(ACabecalho,
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
                     ['return', 'outputXML', 'RecepcionarLoteRpsSincronoResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.GerarNFSe(ACabecalho,
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
                     ['return', 'outputXML', 'GerarNfseResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.ConsultarLote(ACabecalho,
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
                     ['return', 'outputXML', 'ConsultarLoteRpsResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.ConsultarNFSePorFaixa(ACabecalho,
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
                     ['return', 'outputXML', 'ConsultarNfseServicoFaixaResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.ConsultarNFSePorRps(ACabecalho,
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
                     ['return', 'outputXML', 'ConsultarNfsePorRpsResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.ConsultarNFSeServicoPrestado(ACabecalho,
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
                     ['return', 'outputXML', 'ConsultarNfseServicoPrestadoResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.ConsultarNFSeServicoTomado(ACabecalho,
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
                     ['return', 'outputXML', 'ConsultarNfseServicoTomadoResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:CancelarNfse>';
  Request := Request + '<web:cabecalho>' + XmlToStr(ACabecalho) + '</web:cabecalho>';
  Request := Request + '<web:xml>' + XmlToStr(AMSG) + '</web:xml>';
  Request := Request + '</web:CancelarNfse>';

  Result := Executar('http://www.fintel.com.br/WebService/CancelarNfse', Request,
                     ['return', 'outputXML', 'CancelarNfseResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

function TACBrNFSeXWebservicefintelISS.SubstituirNFSe(ACabecalho,
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
                     ['return', 'outputXML', 'SubstituirNfseResult'],
                     ['xmlns:web="http://www.fintel.com.br/WebService"']);
end;

end.
