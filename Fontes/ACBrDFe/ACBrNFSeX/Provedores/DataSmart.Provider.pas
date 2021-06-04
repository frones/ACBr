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

unit DataSmart.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceDataSmart = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderDataSmart = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, DataSmart.GravarXml, DataSmart.LerXml;

{ TACBrNFSeProviderDataSmart }

procedure TACBrNFSeProviderDataSmart.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderDataSmart.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DataSmart.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDataSmart.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DataSmart.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDataSmart.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, GerarNFSe);
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
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceDataSmart.Create(FAOwner, AMetodo, GerarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceDataSmart }

function TACBrNFSeXWebserviceDataSmart.GetDadosUsuario: string;
var
  xPref: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    xPref := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params1
  else
    xPref := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params2;

  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<Username>' + Emitente.WSUser + '</Username>' +
              '<Password>' + Emitente.WSSenha + '</Password>' +
              '<Prefeitura>' + xPref + '</Prefeitura>';
//              '<Prefeitura>' + IntToStr(CodigoMunicipio) + '</Prefeitura>';
  end;
end;

function TACBrNFSeXWebserviceDataSmart.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;
(*
      <dat:GerarNfse soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
         <parameters xsi:type="nfse:GerarNfseEnvio" xmlns:nfse="http://www.abrasf.org.br/nfse.xsd">
            <nfseCabecMsg xsi:type="xsd:string">?</nfseCabecMsg>
            <nfseDadosMsg xsi:type="xsd:string">?</nfseDadosMsg>
            <Username xsi:type="xsd:string">?</Username>
            <Password xsi:type="xsd:string">?</Password>
            <Prefeitura xsi:type="xsd:string">?</Prefeitura>
         </parameters>
      </dat:GerarNfse>
*)

  Request := '<dat:GerarNfse>';
  Request := Request + '<parameters xsi:type="nfse:GerarNfseEnvio" xmlns:nfse="http://www.abrasf.org.br/nfse.xsd">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</parameters>';
  Request := Request + '</dat:GerarNfse>';

  {
  Request := '<dat:GerarNfse><parameters href="#1"/></dat:GerarNfse>';
  Request := Request + '<nfse:GerarNfseEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:GerarNfseEnvio>';
  }
  Result := Executar('http://www.datasmart.com.br/GerarNfse', Request,
                     ['gerarNfseResponse', 'GerarNfseResposta'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
//                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"',
                      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"']);
end;

function TACBrNFSeXWebserviceDataSmart.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:ConsultarNfseFaixa><parameters href="#1"/></dat:ConsultarNfseFaixa>';
  Request := Request + '<nfse:ConsultarNfseFaixaEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:ConsultarNfseFaixaEnvio>';

  Result := Executar('http://www.datasmart.com.br/ConsultarNfseFaixa', Request,
                     ['gerarNfseResponse', 'GerarNfseResposta'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:ConsultarNfsePorRps><parameters href="#1"/></dat:ConsultarNfsePorRps>';
  Request := Request + '<nfse:ConsultarNfsePorRpsEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:ConsultarNfsePorRpsEnvio>';

  Result := Executar('http://www.datasmart.com.br/ConsultarNfsePorRps', Request,
                     ['gerarNfseResponse', 'GerarNfseResposta'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:CancelarNfse><parameters href="#1"/></dat:CancelarNfse>';
  Request := Request + '<nfse:CancelarNfseEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:CancelarNfseEnvio>';

  Result := Executar('http://www.datasmart.com.br/CancelarNfse', Request,
                     ['gerarNfseResponse', 'GerarNfseResposta'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

end.
