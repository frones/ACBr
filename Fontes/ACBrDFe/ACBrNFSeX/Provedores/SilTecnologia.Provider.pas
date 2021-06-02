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

unit SilTecnologia.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSilTecnologia = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderSilTecnologia = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeXWebserviceSilTecnologiaV203 = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProviderSilTecnologiaV203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrXmlBase,
  SilTecnologia.GravarXml, SilTecnologia.LerXml;

{ TACBrNFSeXWebserviceSilTecnologia }

function TACBrNFSeXWebserviceSilTecnologia.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:recepcionarLoteRps>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:recepcionarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologia.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:consultarLoteRps>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:consultarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologia.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:consultarSituacaoLoteRPS>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:consultarSituacaoLoteRPS>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologia.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:consultarNFSePorRPS>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:consultarNFSePorRPS>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologia.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:consultarNfse>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:consultarNfse>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologia.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:cancelarNfse>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</ns2:cancelarNfse>';

  Result := Executar('', Request,
                     ['return'{, 'CancelarNfseResposta'}],
                     ['xmlns:ns2="http://nfse.abrasf.org.br"']);
end;

{ TACBrNFSeProviderSilTecnologia }

procedure TACBrNFSeProviderSilTecnologia.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigAssinar do
  begin
    LoteRps := True;
    IncluirURI := False;
  end;
end;

function TACBrNFSeProviderSilTecnologia.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SilTecnologia.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSilTecnologia.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SilTecnologia.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSilTecnologia.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, CancelarNFSe);
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
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologia.Create(FAOwner, AMetodo, CancelarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeProviderSilTecnologiaV203 }

procedure TACBrNFSeProviderSilTecnologiaV203.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;

    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;
end;

function TACBrNFSeProviderSilTecnologiaV203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SilTecnologiaV203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSilTecnologiaV203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SilTecnologiaV203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSilTecnologiaV203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, SubstituirNFSe);
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
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSilTecnologiaV203.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceSilTecnologiaV203 }

function TACBrNFSeXWebserviceSilTecnologiaV203.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:recepcionarLoteRps>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:recepcionarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:recepcionarLoteRpsSincrono>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:recepcionarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:gerarNfse>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:gerarNfse>';

  Result := Executar('', Request,
                     ['return', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarLoteRps>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfsePorFaixa>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfsePorFaixa>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfsePorRps>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfsePorRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.ConsultarNFSeServicoPrestado(
  ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfseServicoPrestado>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfseServicoPrestado>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.ConsultarNFSeServicoTomado(
  ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfseServicoTomado>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfseServicoTomado>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.Cancelar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:cancelarNfse>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:cancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSilTecnologiaV203.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:substituirNfse>';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</nfse:substituirNfse>';

  Result := Executar('', Request,
                     ['return', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

end.
