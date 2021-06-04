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

unit Fiorilli.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceFiorilli = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
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

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderFiorilli = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Fiorilli.GravarXml, Fiorilli.LerXml;

{ TACBrNFSeProviderFiorilli }

procedure TACBrNFSeProviderFiorilli.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.QuebradeLinha := '\s\n';

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;
end;

function TACBrNFSeProviderFiorilli.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Fiorilli.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFiorilli.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Fiorilli.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFiorilli.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, SubstituirNFSe);
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
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceFiorilli.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceFiorilli }

function TACBrNFSeXWebserviceFiorilli.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<username>' + Emitente.WSUser + '</username>' +
              '<password>' + Emitente.WSSenha + '</password>';
  end;
end;

function TACBrNFSeXWebserviceFiorilli.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:recepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:recepcionarLoteRps>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/recepcionarLoteRps', Request,
                     ['EnviarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:recepcionarLoteRpsSincrono>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:recepcionarLoteRpsSincrono>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/recepcionarLoteRpsSincrono', Request,
                     ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:gerarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:gerarNfse>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/gerarNfse', Request,
                     ['GerarNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarLoteRps>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/consultarLoteRps', Request,
                     ['ConsultarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfsePorFaixa>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfsePorFaixa>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/consultarNfsePorFaixa', Request,
                     ['ConsultarNfseFaixaResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfsePorRps>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/consultarNfsePorRps', Request,
                     ['ConsultarNfseRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfseServicoPrestado>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfseServicoPrestado>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/consultarNfseServicoPrestado', Request,
                     ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfseServicoTomado>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfseServicoTomado>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/consultarNfseServicoTomado', Request,
                     ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:cancelarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:cancelarNfse>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/cancelarNfse', Request,
                     ['CancelarNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:substituirNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:substituirNfse>';

  Result := Executar('http://ws.issweb.fiorilli.com.br/substituirNfse', Request,
                     ['SubstituirNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

end.
