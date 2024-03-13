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

unit Abaco.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceAbaco = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeXWebserviceAbaco204 = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderAbaco = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function DefinirIDLote(const ID: string): string; override;
  end;

  TACBrNFSeProviderAbaco101 = class(TACBrNFSeProviderAbaco)
  protected
    procedure Configuracao; override;

  end;

  TACBrNFSeProviderAbaco204 = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrDFeException, Abaco.GravarXml, Abaco.LerXml;

{ TACBrNFSeXWebserviceAbaco }

function TACBrNFSeXWebserviceAbaco.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:RecepcionarLoteRPS.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:RecepcionarLoteRPS.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ARECEPCIONARLOTERPS.Execute', Request,
                     ['Outputxml', 'EnviarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARLOTERPS.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarSituacaoLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarSituacaoLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARSITUACAOLOTERPS.Execute', Request,
                     ['Outputxml', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfsePorRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarNfsePorRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARNFSEPORRPS.Execute', Request,
                     ['Outputxml', 'ConsultarNfseRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARNFSE.Execute', Request,
                     ['Outputxml', 'ConsultarNfseResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:CancelarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:CancelarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACANCELARNFSE.Execute', Request,
                     ['Outputxml', 'CancelarNfseResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
end;

{ TACBrNFSeProviderAbaco }

procedure TACBrNFSeProviderAbaco.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  SetXmlNameSpace('http://www.e-nfs.com.br');

  with ConfigAssinar do
  begin
    LoteRps := True;
    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := 'V2010';
    VersaoAtrib := '201001';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v2010.xsd');
end;

function TACBrNFSeProviderAbaco.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Abaco.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbaco.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Abaco.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbaco.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAbaco.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderAbaco.DefinirIDLote(const ID: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="L' + ID + '"'
  else
    Result := '';
end;

{ TACBrNFSeProviderAbaco101 }

procedure TACBrNFSeProviderAbaco101.Configuracao;
begin
  inherited Configuracao;

  SetXmlNameSpace('');
end;

{ TACBrNFSeProviderAbaco204 }

procedure TACBrNFSeProviderAbaco204.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meLoteAssincrono;
    ConsultaPorFaixaPreencherNumNfseFinal := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteSincrono := False;
      EnviarUnitario := False;
    end;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    CancelarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '201001';
  end;

  with ConfigMsgDados do
  begin
    GerarPrestadorLoteRps := True;
    DadosCabecalho := GetCabecalho('');
  end;
end;

function TACBrNFSeProviderAbaco204.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Abaco204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbaco204.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Abaco204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbaco204.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAbaco204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceAbaco204 }

function TACBrNFSeXWebserviceAbaco204.Cancelar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_CancelarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_CancelarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CANCELARNFSE.Execute', Request,
                     ['Outputxml', 'CancelarNfseResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARLOTERPS.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfsePorFaixa.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfsePorFaixa.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSEPORFAIXA.Execute', Request,
                     ['Outputxml', 'ConsultarNfsePorFaixaResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfsePorRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfsePorRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSEPORRPS.Execute', Request,
                     ['Outputxml', 'ConsultarNfseRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfseServicoPrestado.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfseServicoPrestado.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSESERVICOPRESTADO.Execute', Request,
                     ['Outputxml', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfseServicoTomado.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfseServicoTomado.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSESERVICOTOMADO.Execute', Request,
                     ['Outputxml', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_RecepcionarLoteRPS.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_RecepcionarLoteRPS.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_RECEPCIONARLOTERPS.Execute', Request,
                     ['Outputxml', 'EnviarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_SubstituirNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg>' + XmlToStr(AMSG) + '</e:Nfsedadosmsg>';
  Request := Request + '</e:A24_SubstituirNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_SUBSTITUIRNFSE.Execute', Request,
                     ['Outputxml', 'SubstituirNfseResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco204.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
end;

end.
