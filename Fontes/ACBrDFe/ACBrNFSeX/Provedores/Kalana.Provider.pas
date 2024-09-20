{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit Kalana.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceKalana = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetChave: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property Chave: string read GetChave;
  end;

  TACBrNFSeProviderKalana = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeXConfiguracoes,
  Kalana.GravarXml, Kalana.LerXml;

{ TACBrNFSeProviderKalana }

procedure TACBrNFSeProviderKalana.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;
  end;
end;

function TACBrNFSeProviderKalana.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Kalana.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderKalana.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Kalana.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderKalana.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceKalana.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceKalana }

function TACBrNFSeXWebserviceKalana.GetChave: string;
begin
  Result := TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAcesso
end;

function TACBrNFSeXWebserviceKalana.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:EnviarLoteRpsEnvio>';
  Request := Request + '<Chave>' + Chave + '</Chave>';
  Request := Request + SeparaDados(AMSG, 'EnviarLoteRpsEnvio');
  Request := Request + '</wsn:EnviarLoteRpsEnvio>';

  Result := Executar('', Request,
                     ['EnviarLoteRpsResposta'],
                     ['xmlns:wsn="https://www.kalana.com.br/wsnfe"']);
end;

function TACBrNFSeXWebserviceKalana.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarLoteRpsEnvio>';
  Request := Request + '<Chave>' + Chave + '</Chave>';
  Request := Request + SeparaDados(AMSG, 'ConsultarLoteRpsEnvio');
  Request := Request + '</wsn:ConsultarLoteRpsEnvio>';

  Result := Executar('', Request,
                     ['ConsultarLoteRpsResposta'],
                     ['xmlns:wsn="https://www.kalana.com.br/wsnfe"']);
end;

function TACBrNFSeXWebserviceKalana.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarSituacaoLoteRpsEnvio>';
  Request := Request + SeparaDados(AMSG, 'ConsultarSituacaoLoteRpsEnvio');
  Request := Request + '<Chave>' + Chave + '</Chave>';
  Request := Request + '</wsn:ConsultarSituacaoLoteRpsEnvio>';

  Result := Executar('', Request,
                     ['ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:wsn="https://www.kalana.com.br/wsnfe"']);
end;

function TACBrNFSeXWebserviceKalana.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfsePorRpsEnvio>';
  Request := Request + '<Chave>' + Chave + '</Chave>';
  Request := Request + SeparaDados(AMSG, 'ConsultarNfseRpsEnvio');
  Request := Request + '</wsn:ConsultarNfsePorRpsEnvio>';

  Result := Executar('', Request,
                     ['ConsultarNfsePorRpsResposta'],
                     ['xmlns:wsn="https://www.kalana.com.br/wsnfe"']);
end;

function TACBrNFSeXWebserviceKalana.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceKalana.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:CancelarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceKalana.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
end;

end.
