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

unit DeISS.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceDeISS = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;
    function GetSoapAction: string;

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

    property Namespace: string read GetNamespace;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderDeISS = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, DeISS.GravarXml, DeISS.LerXml;

{ TACBrNFSeProviderDeISS }

procedure TACBrNFSeProviderDeISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderDeISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DeISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDeISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DeISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDeISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmConsultarNFSeURL:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeURL);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, SubstituirNFSe);
        tmAbrirSessao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, AbrirSessao);
        tmFecharSessao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, FecharSessao);
      else
        // tmTeste
        Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, TesteEnvio);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmConsultarNFSeURL:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeURL);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, SubstituirNFSe);
        tmAbrirSessao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, AbrirSessao);
        tmFecharSessao:
          Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, FecharSessao);
      else
        // tmTeste
        Result := TACBrNFSeXWebserviceDeISS.Create(FAOwner, AMetodo, TesteEnvio);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceDeISS }

function TACBrNFSeXWebserviceDeISS.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'xmlns:tns="' + TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.NameSpace + '"'
  else
    Result := 'xmlns:tns="' + TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.NameSpace + '"';
end;

function TACBrNFSeXWebserviceDeISS.GetSoapAction: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.SoapAction
  else
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.SoapAction;
end;

function TACBrNFSeXWebserviceDeISS.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:RecepcionarLoteRpsRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:GerarNfseRequest>';

  Result := Executar(SoapAction + 'GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarLoteRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfsePorFaixaRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfsePorRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:ConsultarNfseServicoTomadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:CancelarNfseRequest>';

  Result := Executar(SoapAction + 'CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceDeISS.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tns:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</tns:SubstituirNfseRequest>';

  Result := Executar(SoapAction + 'SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     [NameSpace]);
end;

end.
