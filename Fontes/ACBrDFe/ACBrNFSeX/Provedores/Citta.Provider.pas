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

unit Citta.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeX, ACBrNFSeXClass, ACBrNFSeXConversao, ACBrNFSeXConfiguracoes,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceCitta = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProviderCitta = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrDFeException,
  Citta.GravarXml, Citta.LerXml;

{ TACBrNFSeProviderCitta }

procedure TACBrNFSeProviderCitta.Configuracao;
begin
  inherited Configuracao;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
    AtribVerLote := 'versao';
  end;
end;

function TACBrNFSeProviderCitta.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Citta.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCitta.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Citta.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCitta.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceCitta.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderCitta.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  Xml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  Xml := Response.XmlEnvio;

  Xml := StringReplace(Xml,
              ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

  case aMetodo of
    tmRecepcionar:
      begin
        Xml := StringReplace(Xml,
              'EnviarLoteRpsEnvio', 'nfse:RecepcionarLoteRpsEnvio', [rfReplaceAll]);
        Xml := StringReplace(Xml,
                            '<Rps><InfDecla', '<rps><InfDecla', [rfReplaceAll]);
        Xml := StringReplace(Xml,
                            'Servico></Rps>', 'Servico></rps>', [rfReplaceAll]);
      end;

    tmRecepcionarSincrono:
      begin
        Xml := StringReplace(Xml,
              'EnviarLoteRpsSincronoEnvio', 'nfse:RecepcionarLoteRpsSincronoEnvio', [rfReplaceAll]);
        Xml := StringReplace(Xml,
                            '<Rps><InfDecla', '<rps><InfDecla', [rfReplaceAll]);
        Xml := StringReplace(Xml,
                            'Servico></Rps>', 'Servico></rps>', [rfReplaceAll]);
      end;

    tmGerar:
      begin
        Xml := StringReplace(Xml,
              'GerarNfseEnvio', 'nfse:GerarNfseEnvio', [rfReplaceAll]);
      end;

    tmConsultarLote:
      begin
        Xml := StringReplace(Xml,
              'ConsultarLoteRpsEnvio', 'nfse:ConsultarLoteRpsEnvio', [rfReplaceAll]);
      end;

    tmConsultarNFSePorRps:
      begin
        Xml := StringReplace(Xml,
              'ConsultarNfseRpsEnvio', 'nfse:ConsultarNfseRpsEnvio', [rfReplaceAll]);
      end;

    tmConsultarNFSePorFaixa:
      begin
        Xml := StringReplace(Xml,
              'ConsultarNfseFaixaEnvio', 'nfse:ConsultarNfsePorFaixaEnvio', [rfReplaceAll]);
      end;

    tmConsultarNFSeServicoPrestado:
      begin
        Xml := StringReplace(Xml,
              'ConsultarNfseServicoPrestadoEnvio', 'nfse:ConsultarNfseServicoPrestadoEnvio', [rfReplaceAll]);
      end;

    tmConsultarNFSeServicoTomado:
      begin
        Xml := StringReplace(Xml,
              'ConsultarNfseServicoTomadoEnvio', 'nfse:ConsultarNfseServicoTomadoEnvio', [rfReplaceAll]);
      end;

    tmCancelarNFSe:
      begin
        Xml := StringReplace(Xml,
              'CancelarNfseEnvio', 'nfse:CancelarNfseEnvio', [rfReplaceAll]);
      end;

    tmSubstituirNFSe:
      begin
        Xml := StringReplace(Xml,
              'SubstituirNfseEnvio', 'nfse:SubstituirNfseEnvio', [rfReplaceAll]);
      end;
  else
    Response.XmlEnvio := Xml;
  end;

  Response.XmlEnvio := Xml;
end;

{ TACBrNFSeXWebserviceCitta }

function TACBrNFSeXWebserviceCitta.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.GerarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', AMSG,
                     [''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorFaixa', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceCitta.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', AMSG,
                     [''], ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

end.
