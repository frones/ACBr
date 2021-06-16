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
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeXWebserviceAbacov204 = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderAbaco = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeProviderAbacoA = class(TACBrNFSeProviderAbaco)
  protected
    procedure Configuracao; override;

  end;

  TACBrNFSeProviderAbacov204 = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrDFeException, Abaco.GravarXml, Abaco.LerXml;

{ TACBrNFSeXWebserviceAbaco }

function TACBrNFSeXWebserviceAbaco.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:RecepcionarLoteRPS.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:RecepcionarLoteRPS.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ARECEPCIONARLOTERPS.Execute', Request,
                     ['Outputxml', 'EnviarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARLOTERPS.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarSituacaoLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarSituacaoLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARSITUACAOLOTERPS.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfsePorRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarNfsePorRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARNFSEPORRPS.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:ConsultarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACONSULTARNFSE.Execute', Request,
                     ['Outputxml', 'ConsultarLoteRpsResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

function TACBrNFSeXWebserviceAbaco.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:CancelarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:CancelarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/ACANCELARNFSE.Execute', Request,
                     ['Outputxml', 'CancelarNfseResposta'],
                     ['xmlns:e="http://www.e-nfs.com.br"']);
end;

{ TACBrNFSeProviderAbaco }

procedure TACBrNFSeProviderAbaco.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  SetXmlNameSpace('http://www.e-nfs.com.br');

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
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeProviderAbacoA }

procedure TACBrNFSeProviderAbacoA.Configuracao;
begin
  inherited Configuracao;

  SetXmlNameSpace('');
end;

{ TACBrNFSeProviderAbacov204 }

procedure TACBrNFSeProviderAbacov204.Configuracao;
begin
  inherited Configuracao;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '201001';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderAbacov204.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Abacov204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbacov204.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Abacov204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAbacov204.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAbacov204.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceAbacov204 }

function TACBrNFSeXWebserviceAbacov204.Cancelar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_CancelarNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_CancelarNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CANCELARNFSE.Execute', Request,
                     'A24_CancelarNfse.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarLoteRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarLoteRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARLOTERPS.Execute', Request,
                     'A24_ConsultarLoteRps.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfsePorFaixa.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfsePorFaixa.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSEPORFAIXA.Execute', Request,
                     'A24_ConsultarNfsePorFaixa.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfsePorRps.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfsePorRps.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSEPORRPS.Execute', Request,
                     'A24_ConsultarNfsePorRps.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfseServicoPrestado.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfseServicoPrestado.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSESERVICOPRESTADO.Execute', Request,
                     'A24_ConsultarNfseServicoPrestado.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_ConsultarNfseServicoTomado.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_ConsultarNfseServicoTomado.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_CONSULTARNFSESERVICOTOMADO.Execute', Request,
                     'A24_ConsultarNfseServicoTomado.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_RecepcionarLoteRPS.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_RecepcionarLoteRPS.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_RECEPCIONARLOTERPS.Execute', Request,
                     'A24_RecepcionarLoteRPS.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

function TACBrNFSeXWebserviceAbacov204.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:A24_SubstituirNfse.Execute>';
  Request := Request + '<e:Nfsecabecmsg><![CDATA[' + ACabecalho + ']]></e:Nfsecabecmsg>';
  Request := Request + '<e:Nfsedadosmsg><![CDATA[' + AMSG + ']]></e:Nfsedadosmsg>';
  Request := Request + '</e:A24_SubstituirNfse.Execute>';

  Result := Executar('http://www.e-nfs.com.braction/AA24_SUBSTITUIRNFSE.Execute', Request,
                     'A24_SubstituirNfse.ExecuteResponse',
                     'xmlns:e="http://www.e-nfs.com.br"');
end;

end.
