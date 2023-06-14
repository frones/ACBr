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

unit Tinus.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceTinus = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
    function GetSoapAction: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property NameSpace: string read GetNameSpace;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderTinus = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

  TACBrNFSeProviderTinus102 = class (TACBrNFSeProviderTinus)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrXmlBase, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Tinus.GravarXml, Tinus.LerXml;

{ TACBrNFSeXWebserviceTinus }

function TACBrNFSeXWebserviceTinus.GetNameSpace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'http://www.tinus.com.br'
  else
    Result := 'http://www2.tinus.com.br';

  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Versao = ve102 then
    Result := 'http://www.abrasf.org.br/nfse.xsd';

  Result := 'xmlns:tin="' + Result + '"';
end;

function TACBrNFSeXWebserviceTinus.GetSoapAction: string;
begin
  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Versao = ve102 then
    Result := 'http://www.abrasf.org.br/nfse.xsd/WSNFSE.'
  else
    Result := 'http://www.tinus.com.br/WSNFSE.';
end;

function TACBrNFSeXWebserviceTinus.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:RecepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</tin:RecepcionarLoteRps>';

  Result := Executar(SoapAction + 'RecepcionarLoteRps.RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceTinus.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:ConsultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</tin:ConsultarLoteRps>';

  Result := Executar(SoapAction + 'ConsultarLoteRps.ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResult'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceTinus.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:ConsultarSituacaoLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</tin:ConsultarSituacaoLoteRps>';

  Result := Executar(SoapAction + 'ConsultarSituacaoLoteRps.ConsultarSituacaoLoteRps', Request,
                     ['ConsultarSituacaoLoteRpsResult'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceTinus.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:ConsultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + '</tin:ConsultarNfsePorRps>';

  Result := Executar(SoapAction + 'ConsultarNfsePorRps.ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResult'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceTinus.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:ConsultarNfse>';
  Request := Request + AMSG;
  Request := Request + '</tin:ConsultarNfse>';

  Result := Executar(SoapAction + 'ConsultarNfse.ConsultarNfse', Request,
                     ['ConsultarNfseResult'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceTinus.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tin:CancelarNfse>';
  Request := Request + AMSG;
  Request := Request + '</tin:CancelarNfse>';

  Result := Executar(SoapAction + 'CancelarNfse.CancelarNfse', Request,
                     ['CancelarNfseResult'],
                     [NameSpace]);
end;

{ TACBrNFSeProviderTinus }

procedure TACBrNFSeProviderTinus.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
  end;

  SetXmlNameSpace('http://www.tinus.com.br');

  SetNomeXSD('nfsetinus.xsd');
end;

function TACBrNFSeProviderTinus.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Tinus.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderTinus.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Tinus.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderTinus.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceTinus.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderTinus.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  xXml := Response.ArquivoEnvio;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
    xXml := StringReplace(xXml, 'www.tinus', 'www2.tinus', [rfReplaceAll]);

  if ConfigGeral.Versao = ve100 then
  begin
    case aMetodo of
        tmRecepcionar:
          xXml := StringReplace(xXml, 'EnviarLoteRpsEnvio', 'Arg', [rfReplaceAll]);

        tmConsultarSituacao:
          xXml := StringReplace(xXml, 'ConsultarSituacaoLoteRpsEnvio', 'Arg', [rfReplaceAll]);

        tmConsultarLote:
          xXml := StringReplace(xXml, 'ConsultarLoteRpsEnvio', 'Arg', [rfReplaceAll]);

        tmConsultarNFSePorRps:
          xXml := StringReplace(xXml, 'ConsultarNfseRpsEnvio', 'Arg', [rfReplaceAll]);

        tmConsultarNFSe:
          xXml := StringReplace(xXml, 'ConsultarNfseEnvio', 'Arg', [rfReplaceAll]);

        tmCancelarNFSe:
          xXml := StringReplace(xXml, 'CancelarNfseEnvio', 'Arg', [rfReplaceAll]);
    else
      Response.ArquivoEnvio := xXml;
    end;
  end;

  Response.ArquivoEnvio := xXml;
end;

{ TACBrNFSeProviderTinus102 }

procedure TACBrNFSeProviderTinus102.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'Id';

  SetXmlNameSpace('http://www.abrasf.org.br/nfse.xsd');

  SetNomeXSD('nfse.xsd');
end;

end.
