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

unit Thema.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceThema = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderThema = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Thema.GravarXml, Thema.LerXml;

{ TACBrNFSeXWebserviceThema }

function TACBrNFSeXWebserviceThema.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  // Para o provedor Thema devemos utilizar esse serviço caso a quantidade
  // de RPS no lote seja superior a 3, ou seja, 4 ou mais
  FPMsgOrig := AMSG;

//      IncluirEncodingDados := True;
  Request := '<recepcionarLoteRps xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</recepcionarLoteRps>';

  Result := Executar('urn:recepcionarLoteRps', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  // Para o provedor Thema devemos utilizar esse serviço caso a quantidade
  // de RPS no lote seja inferior a 4.
  FPMsgOrig := AMSG;

//      IncluirEncodingDados := True;
  Request := '<recepcionarLoteRpsLimitado xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</recepcionarLoteRpsLimitado>';

  Result := Executar('urn:recepcionarLoteRpsLimitado', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<consultarLoteRps xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</consultarLoteRps>';

  Result := Executar('urn:consultarLoteRps', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<consultarSituacaoLoteRps xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</consultarSituacaoLoteRps>';

  Result := Executar('urn:consultarSituacaoLoteRps', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<consultarNfsePorRps xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</consultarNfsePorRps>';

  Result := Executar('urn:consultarNfsePorRps', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<consultarNfse xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</consultarNfse>';

  Result := Executar('urn:consultarNfse', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceThema.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<cancelarNfse xmlns="http://server.nfse.thema.inf.br">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</cancelarNfse>';

  Result := Executar('urn:cancelarNfse', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['']);
end;

{ TACBrNFSeProviderThema }

procedure TACBrNFSeProviderThema.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
  end;

  ConfigMsgDados.LoteRpsSincrono.DocElemento := 'EnviarLoteRpsEnvio';
end;

function TACBrNFSeProviderThema.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Thema.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderThema.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Thema.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderThema.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, RecepcionarSincrono);
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
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceThema.Create(FAOwner, AMetodo, RecepcionarSincrono);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

end.
