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

unit ISSCuritiba.Provider;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSCuritiba = class(TACBrNFSeXWebserviceSoap12)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderISSCuritiba = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrXmlBase, ACBrDFeException,
  ISSCuritiba.GravarXml, ISSCuritiba.LerXml;

{ TACBrNFSeProviderISSCuritiba }

procedure TACBrNFSeProviderISSCuritiba.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  SetXmlNameSpace('http://isscuritiba.curitiba.pr.gov.br/iss/nfse.xsd');

  ConfigAssinar.LoteRps := True;
end;

function TACBrNFSeProviderISSCuritiba.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSCuritiba.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCuritiba.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSCuritiba.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCuritiba.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSCuritiba.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceISSCuritiba }

function TACBrNFSeXWebserviceISSCuritiba.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('https://www.e-governeapps2.com.br/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult'], []);
end;

function TACBrNFSeXWebserviceISSCuritiba.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('https://www.e-governeapps2.com.br/ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResult'], []);
end;

function TACBrNFSeXWebserviceISSCuritiba.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarSituacaoLoteRps xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</ConsultarSituacaoLoteRps>';

  Result := Executar('https://www.e-governeapps2.com.br/ConsultarSituacaoLoteRps', Request,
                     ['ConsultarSituacaoLoteRpsResult'], []);
end;

function TACBrNFSeXWebserviceISSCuritiba.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfsePorRps xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</ConsultarNfsePorRps>';

  Result := Executar('https://www.e-governeapps2.com.br/ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResult'], []);
end;

function TACBrNFSeXWebserviceISSCuritiba.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfse xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</ConsultarNfse>';

  Result := Executar('https://www.e-governeapps2.com.br/ConsultarNfse', Request,
                     ['ConsultarNfseResult'], []);
end;

function TACBrNFSeXWebserviceISSCuritiba.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse xmlns="https://www.e-governeapps2.com.br/">';
  Request := Request + AMSG;
  Request := Request + '</CancelarNfse>';

  Result := Executar('https://www.e-governeapps2.com.br/CancelarNfse', Request,
                     ['CancelarNfseResult'], []);
end;

end.
