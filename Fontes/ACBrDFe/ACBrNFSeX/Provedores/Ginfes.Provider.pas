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

{
     Funcionou informando httpWinINet para HTTPLib
}

{$I ACBr.inc}

unit Ginfes.Provider;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceGinfes = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property NameSpace: string read GetNameSpace;
  end;

  TACBrNFSeProviderGinfes = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Ginfes.GravarXml, Ginfes.LerXml;

{ TACBrNFSeProviderGinfes }

procedure TACBrNFSeProviderGinfes.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.QuebradeLinha := '\n';

  with ConfigMsgDados do
  begin
    Prefixo := 'ns3';
    PrefixoTS := 'ns4';

    DadosCabecalho := '<ns2:cabecalho versao="3" xmlns:ns2="http://www.ginfes.com.br/cabecalho_v03.xsd">' +
                      '<versaoDados>3</versaoDados>' +
                      '</ns2:cabecalho>';

    XmlRps.xmlns := 'http://www.ginfes.com.br/tipos_v03.xsd';

    LoteRps.xmlns := 'http://www.ginfes.com.br/servico_enviar_lote_rps_envio_v03.xsd';

    ConsultarSituacao.xmlns := 'http://www.ginfes.com.br/servico_consultar_situacao_lote_rps_envio_v03.xsd';

    ConsultarLote.xmlns := 'http://www.ginfes.com.br/servico_consultar_lote_rps_envio_v03.xsd';

    ConsultarNFSeRps.xmlns := 'http://www.ginfes.com.br/servico_consultar_nfse_rps_envio_v03.xsd';

    ConsultarNFSe.xmlns := 'http://www.ginfes.com.br/servico_consultar_nfse_envio_v03.xsd';

    with CancelarNFSe do
    begin
      xmlns := 'http://www.ginfes.com.br/servico_cancelar_nfse_envio';
      InfElemento := 'Prestador';
      DocElemento := 'CancelarNfseEnvio';
    end;
  end;

  with ConfigAssinar do
  begin
    LoteRps           := True;
    ConsultarSituacao := True;
    ConsultarLote     := True;
    ConsultarNFSeRps  := True;
    ConsultarNFSe     := True;
    CancelarNFSe      := True;
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio_v03.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio_v03.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio_v03.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio_v03.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio_v03.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio_v02.xsd';
  end;
end;

function TACBrNFSeProviderGinfes.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Ginfes.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGinfes.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Ginfes.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGinfes.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, CancelarNFSe);
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
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSe:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, ConsultarNFSe);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceGinfes.Create(FAOwner, AMetodo, CancelarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderGinfes.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  IdAttr, NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  InfoCanc := Response.InfCancelamento;

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Canc_' +
                      OnlyNumber(Emitente.CNPJ) + OnlyNumber(Emitente.InscMun) +
                      InfoCanc.NumeroNFSe + '"'
  else
    IdAttr := '';

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.CancelarNFSe.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                            ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  NameSpace := StringReplace(NameSpace, '_v03.xsd', '', [rfReplaceAll]);

  Response.XmlEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                         '<' + Prefixo + 'Prestador>' +
                           '<' + PrefixoTS + 'Cnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</' + PrefixoTS + 'Cnpj>' +
                           GetInscMunic(Emitente.InscMun, PrefixoTS) +
                         '</' + Prefixo + 'Prestador>' +
                         '<' + Prefixo + 'NumeroNfse>' +
                           InfoCanc.NumeroNFSe +
                         '</' + Prefixo + 'NumeroNfse>' +
                       '</' + Prefixo + 'CancelarNfseEnvio>';
end;

{ TACBrNFSeXWebserviceGinfes }

function TACBrNFSeXWebserviceGinfes.GetNameSpace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := 'http://homologacao.ginfes.com.br'
  else
    Result := 'http://producao.ginfes.com.br';

  Result := ' xmlns:ns1="' + Result + '"';
end;

function TACBrNFSeXWebserviceGinfes.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:RecepcionarLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:RecepcionarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarSituacaoLoteRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarSituacaoLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfsePorRpsV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarNfsePorRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceGinfes.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfseV3' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + XmlToStr(AMSG) + '</arg1>';
  Request := Request + '</ns1:ConsultarNfseV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['']);
end;

function TACBrNFSeXWebserviceGinfes.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:CancelarNfse' + NameSpace + '>';
  Request := Request + '<arg0>' + XmlToStr(AMSG) + '</arg0>';
  Request := Request + '</ns1:CancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['']);
end;

end.
