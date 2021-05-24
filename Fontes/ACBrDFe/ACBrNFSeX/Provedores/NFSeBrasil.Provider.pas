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

unit NFSeBrasil.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceNFSeBrasil = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDatosUsuario: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property DadosUsuario: string read GetDatosUsuario;
  end;

  TACBrNFSeProviderNFSeBrasil = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = 'erros';
                                     AMessageTag: string = 'erro'); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrXmlBase,
  ACBrNFSeXNotasFiscais, NFSeBrasil.GravarXml, NFSeBrasil.LerXml;

{ TACBrNFSeXWebserviceNFSeBrasil }

function TACBrNFSeXWebserviceNFSeBrasil.GetDatosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<codMunicipio>' + IntToStr(CodigoMunicipio) + '</codMunicipio>' +
              '<cnpjPrestador>' + Emitente.WSUser + '</cnpjPrestador>' +
              '<hashValidador>' + Emitente.WSSenha + '</hashValidador>';
  end;
end;

function TACBrNFSeXWebserviceNFSeBrasil.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request, xXml, CodMun: string;
begin
  FPMsgOrig := AMSG;

  CodMun := IntToStr(TACBrNFSeX(FPDFeOwner).Configuracoes.Geral.CodigoMunicipio);
  xXml := StringReplace(AMSG, 'versao="1.00"',
    'versao="1.00" codMunicipio="' + CodMun + '"', [rfReplaceAll]);

  Request := '<urn:tm_lote_rps_service.importarLoteRPS>';
  Request := Request + '<xml>' + XmlToStr(xXml) + '</xml>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.importarLoteRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.importarLoteRPS', Request,
                     ['return', 'RespostaLoteRps'],
                     ['xmlns:urn="urn:loterpswsdl"']);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarLoteRPS>';
  Request := Request + '<protocolo>' + XmlToStr(AMSG) + '</protocolo>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarLoteRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarLoteRPS', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:urn="urn:loterpswsdl"']);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarRPS>';
  Request := Request + '<numeroRPS>' + XmlToStr(AMSG) + '</numeroRPS>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarRPS', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:urn="urn:loterpswsdl"']);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarNFSE>';
  Request := Request + '<numeroNFSE>' + XmlToStr(AMSG) + '</numeroNFSE>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarNFSE>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarNFSE', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:urn="urn:loterpswsdl"']);
end;

function TACBrNFSeXWebserviceNFSeBrasil.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.cancelarNFSE>';
  Request := Request + '<numeroNFSE>' + XmlToStr(AMSG) + '</numeroNFSE>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.cancelarNFSE>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.cancelarNFSE', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:urn="urn:loterpswsdl"']);
end;

{ TACBrNFSeProviderNFSeBrasil }

procedure TACBrNFSeProviderNFSeBrasil.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  SetXmlNameSpace('http://www.nfsebrasil.net.br/nfse/rps/xsd/rps.xsd');

  ConfigMsgDados.ConsultarNFSe.DocElemento := 'ConsultarNfsePorRpsEnvio';

  ConfigWebServices.AtribVerLote := 'versao';

  SetNomeXSD('rps.xsd');
  // Não tem todos os XSD para todos os serviços
  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderNFSeBrasil.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_NFSeBrasil.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFSeBrasil.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_NFSeBrasil.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFSeBrasil.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmConsultarNFSeURL:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeURL);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, SubstituirNFSe);
        tmAbrirSessao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, AbrirSessao);
        tmFecharSessao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, FecharSessao);
      else
        // tmTeste
        Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, TesteEnvio);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmConsultarNFSeURL:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeURL);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, SubstituirNFSe);
        tmAbrirSessao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, AbrirSessao);
        tmFecharSessao:
          Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, FecharSessao);
      else
        // tmTeste
        Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, TesteEnvio);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
begin
  Response.XmlEnvio := Response.Protocolo;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
begin
  Response.XmlEnvio := Response.NumRPS;
end;

procedure TACBrNFSeProviderNFSeBrasil.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I: Integer;
//  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
//  ANode := RootNode.Childrens.FindAnyNs(AListTag);

//  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);
  ANodeArray := RootNode.Childrens.FindAllAnyNs('erros');

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[i].Childrens.FindAnyNs('erro'), tcStr);
    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
begin
  Response.Metodo := tmConsultarNFSe;
  Response.XmlEnvio := Response.InfConsultaNFSe.NumeroIniNFSe;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
begin
  Response.XmlEnvio := Response.InfCancelamento.NumeroNFSe;
end;

end.
