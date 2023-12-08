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

unit SystemPro.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSystemPro201 = class(TACBrNFSeXWebserviceSoap11)
  public
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSystemPro201 = class (TACBrNFSeProviderABRASFv2)
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
  SystemPro.GravarXml, SystemPro.LerXml;

{ TACBrNFSeProviderSystemPro201 }

procedure TACBrNFSeProviderSystemPro201.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal := True;

  with ConfigGeral.ServicosDisponibilizados do
  begin
    EnviarLoteAssincrono := False;
    ConsultarLote := False;
    ConsultarRps := False;
    ConsultarServicoPrestado := False;
    ConsultarServicoTomado := False;
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '2.01';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderSystemPro201.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SystemPro201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSystemPro201.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SystemPro201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSystemPro201.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSystemPro201.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceSystemPro201 }

function TACBrNFSeXWebserviceSystemPro201.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:EnviarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns2:EnviarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:ns2="http://NFSe.wsservices.systempro.com.br/"']);
end;

function TACBrNFSeXWebserviceSystemPro201.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns2:GerarNfse>';

  Result := Executar('', Request,
                     ['return', 'GerarNfseResposta'],
                     ['xmlns:ns2="http://NFSe.wsservices.systempro.com.br/"']);
end;

function TACBrNFSeXWebserviceSystemPro201.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:ConsultarNfseFaixa>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns2:ConsultarNfseFaixa>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:ns2="http://NFSe.wsservices.systempro.com.br/"']);
end;

function TACBrNFSeXWebserviceSystemPro201.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns2:CancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:ns2="http://NFSe.wsservices.systempro.com.br/"']);
end;

function TACBrNFSeXWebserviceSystemPro201.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns2:SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns2:SubstituirNfse>';

  Result := Executar('', Request,
                     ['return', 'SubstituirNfseResposta'],
                     ['xmlns:ns2="http://NFSe.wsservices.systempro.com.br/"']);
end;

function TACBrNFSeXWebserviceSystemPro201.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

end.
