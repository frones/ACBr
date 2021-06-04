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

unit Link3.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceLink3 = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProviderLink3 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Link3.GravarXml, Link3.LerXml;

{ TACBrNFSeProviderLink3 }

procedure TACBrNFSeProviderLink3.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;
end;

function TACBrNFSeProviderLink3.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Link3.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderLink3.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Link3.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderLink3.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, SubstituirNFSe);
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
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceLink3.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceLink3 }

function TACBrNFSeXWebserviceLink3.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:recepcionarLoteRps>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:recepcionarLoteRps>';

  Result := Executar('recepcionarLoteRps', Request,
                     ['return', 'outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:recepcionarLoteRpsSincrono>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:recepcionarLoteRpsSincrono>';

  Result := Executar('recepcionarLoteRpsSincrono', Request,
                     ['return', 'outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:gerarNfse>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:gerarNfse>';

  Result := Executar('gerarNfse', Request,
                     ['return', 'outputXML', 'GerarNfseResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:consultarLoteRps>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:consultarLoteRps>';

  Result := Executar('consultarLoteRps', Request,
                     ['return', 'outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:consultarNfsePorFaixa>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:consultarNfsePorFaixa>';

  Result := Executar('consultarNfsePorFaixa', Request,
                     ['return', 'outputXML', 'ConsultarNfsePorFaixaResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:consultarNfsePorRps>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:consultarNfsePorRps>';

  Result := Executar('consultarNfsePorRps', Request,
                     ['return', 'outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:consultarNfseServicoPrestado>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:consultarNfseServicoPrestado>';

  Result := Executar('consultarNfseServicoPrestado', Request,
                     ['return', 'outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:consultarNfseServicoTomado>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:consultarNfseServicoTomado>';

  Result := Executar('consultarNfseServicoTomado', Request,
                     ['return', 'outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:cancelarNfse>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:cancelarNfse>';

  Result := Executar('cancelarNfse', Request,
                     ['return', 'outputXML', 'CancelarNfseResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

function TACBrNFSeXWebserviceLink3.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:substituirNfse>';
  Request := Request + XmlToStr(AMSG);
  Request := Request + '</tns:substituirNfse>';

  Result := Executar('substituirNfse', Request,
                     ['return', 'outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:tns="http://impl.nfse.services.l3grp.link3.com.br/"']);
end;

end.
