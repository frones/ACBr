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

unit DSFSJC.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceDSFSJC = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function AlterarNameSpace(aMsg: string): string;
  end;

  TACBrNFSeProviderDSFSJC = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, DSFSJC.GravarXml, DSFSJC.LerXml;

{ TACBrNFSeXWebserviceDSFSJC }

function TACBrNFSeXWebserviceDSFSJC.AlterarNameSpace(aMsg: string): string;
begin
  Result := StringReplace(aMsg, 'http://www.abrasf.org.br/nfse.xsd',
                            'http:/www.abrasf.org.br/nfse.xsd', [rfReplaceAll]);
end;

function TACBrNFSeXWebserviceDSFSJC.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:RecepcionarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDSFSJC.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:ConsultarLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDSFSJC.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarSituacaoLoteRpsV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:ConsultarSituacaoLoteRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDSFSJC.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRpsV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:ConsultarNfsePorRpsV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDSFSJC.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:ConsultarNfseV3>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDSFSJC.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseV3>';
  Request := Request + '<arg0>' + IncluirCDATA(ACabecalho) + '</arg0>';
  Request := Request + '<arg1>' + IncluirCDATA(AlterarNameSpace(AMSG)) + '</arg1>';
  Request := Request + '</nfse:CancelarNfseV3>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

{ TACBrNFSeProviderDSFSJC }

procedure TACBrNFSeProviderDSFSJC.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
  end;

  SetXmlNameSpace('http://www.abrasf.org.br/nfse.xsd');

  ConfigWebServices.AtribVerLote := 'versao';

  ConfigMsgDados.DadosCabecalho := '<ns2:cabecalho versao="3" xmlns:ns2="http:/www.abrasf.org.br/nfse.xsd">' +
                                   '<versaoDados>3</versaoDados>' +
                                   '</ns2:cabecalho>';
end;

function TACBrNFSeProviderDSFSJC.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DSFSJC.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSFSJC.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DSFSJC.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSFSJC.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceDSFSJC.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

end.
