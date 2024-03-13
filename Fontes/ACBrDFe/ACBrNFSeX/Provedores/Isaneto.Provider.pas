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

unit Isaneto.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceIsaneto203 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderIsaneto203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrDFeException,
  Isaneto.GravarXml, Isaneto.LerXml;

{ TACBrNFSeProviderIsaneto203 }

procedure TACBrNFSeProviderIsaneto203.Configuracao;
const
  NameSpace = 'http://www.abrasf.org.br/nfse.xsd" xmlns:ns2="http://www.w3.org/2000/09/xmldsig#';
begin
  inherited Configuracao;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace(NameSpace);

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;
end;

function TACBrNFSeProviderIsaneto203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Isaneto203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIsaneto203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Isaneto203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIsaneto203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIsaneto203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceIsaneto203 }

function TACBrNFSeXWebserviceIsaneto203.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  {
  Request := '<nfse:RecepcionarLoteRpsRequest>';
  Request := Request + SeparaDados(AMSG, 'EnviarLoteRpsEnvio');
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
  }
  Request := '<nfse:RecepcionarLoteRpsRequest xmlns="http://www.abrasf.org.br/nfse.xsd" xmlns:ns2="http://www.w3.org/2000/09/xmldsig#">';
  Request := Request + SeparaDados(AMSG, 'EnviarLoteRpsEnvio');
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns:nfse1="http://www.abrasf.org.br/nfse.xsd"',
                      'xmlns:xd="http://www.w3.org/2000/09/xmldsig#"']);
end;

function TACBrNFSeXWebserviceIsaneto203.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + SeparaDados(AMSG, 'EnviarLoteRpsSincronoEnvio');
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest>';
  Request := Request + SeparaDados(AMSG, 'GerarNfseEnvio');
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest>';
  Request := Request + SeparaDados(AMSG, 'ConsultarLoteRpsEnvio');
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixaRequest>';
  Request := Request + SeparaDados(AMSG, 'ConsultarNfseFaixaEnvio');
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRpsRequest>';
  Request := Request + SeparaDados(AMSG, 'ConsultarNfseRpsEnvio');
  Request := Request + '</nfse:ConsultarNfsePorRpsRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + SeparaDados(AMSG, 'ConsultarNfseServicoPrestadoEnvio');
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + SeparaDados(AMSG, 'ConsultarNfseServicoTomadoEnvio');
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest>';
  Request := Request + SeparaDados(AMSG, 'CancelarNfseEnvio');
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest>';
  Request := Request + SeparaDados(AMSG, 'SubstituirNfseEnvio');
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar('', Request, ['', ''],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"',
                      'xmlns="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceIsaneto203.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
  Result := TiraAcentos(Result);
end;

end.
