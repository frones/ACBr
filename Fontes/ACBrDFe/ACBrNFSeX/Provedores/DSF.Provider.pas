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

unit DSF.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceDSF = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function AlterarNameSpace(const aMsg: string): string;
    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderDSF = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeXWebserviceDSF200 = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProviderDSF200 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

  TACBrNFSeProviderDSF203 = class (TACBrNFSeProviderDSF200)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  DSF.GravarXml, DSF.LerXml;

{ TACBrNFSeXWebserviceDSF }

function TACBrNFSeXWebserviceDSF.AlterarNameSpace(const aMsg: string): string;
begin
  Result := StringReplace(aMsg, 'http://www.abrasf.org.br/nfse.xsd',
                            'http:/www.abrasf.org.br/nfse.xsd', [rfReplaceAll]);
end;

function TACBrNFSeXWebserviceDSF.Recepcionar(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.ConsultarLote(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.ConsultarSituacao(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.ConsultarNFSe(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.Cancelar(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceDSF.TratarXmlRetornado(const aXML: string): string;
var
  Xml: string;
begin
  Xml := ConverteANSIparaUTF8(aXML);
  Xml := RemoverDeclaracaoXML(Xml);

  Result := inherited TratarXmlRetornado(Xml);

  Result := ParseText(Result);
//  Result := RemoverDeclaracaoXML(Result);
end;

{ TACBrNFSeProviderDSF }

procedure TACBrNFSeProviderDSF.Configuracao;
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

function TACBrNFSeProviderDSF.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DSF.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSF.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DSF.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSF.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceDSF.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceDSF200 }

function TACBrNFSeXWebserviceDSF200.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('', Request,
                     ['EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincrono>';
  Request := Request + AMSG;
  Request := Request + '</nfse:RecepcionarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:GerarNfse>';

  Result := Executar('', Request,
                     ['GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('', Request,
                     ['ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseFaixa>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfseFaixa>';

  Result := Executar('', Request,
                     ['ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('', Request,
                     ['ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestado>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfseServicoPrestado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomado>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfseServicoTomado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('', Request,
                     ['CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:SubstituirNfse>';

  Result := Executar('', Request,
                     ['SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceDSF200.TratarXmlRetornado(
  const aXML: string): string;
var
  Xml: string;
begin
  Xml := ConverteANSIparaUTF8(aXML);
  Xml := RemoverDeclaracaoXML(Xml);

  Result := inherited TratarXmlRetornado(Xml);

  Result := ParseText(Result);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

{ TACBrNFSeProviderDSF200 }

procedure TACBrNFSeProviderDSF200.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.QuebradeLinha := '&#xD;&#xA;';

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
  end;
end;

function TACBrNFSeProviderDSF200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DSF200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSF200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DSF200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDSF200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceDSF200.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderDSF200.ValidarSchema(Response: TNFSeWebserviceResponse;
  aMetodo: TMetodo);
begin
  inherited ValidarSchema(Response, aMetodo);

  Response.ArquivoEnvio := StringReplace(Response.ArquivoEnvio,
                          ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', []);
end;

{ TACBrNFSeProviderDSF203 }

procedure TACBrNFSeProviderDSF203.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  SetXmlNameSpace('');
end;

end.
