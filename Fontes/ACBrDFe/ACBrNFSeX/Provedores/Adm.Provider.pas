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

unit Adm.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceAdm201 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    property Namespace: string read GetNamespace;
  end;

  TACBrNFSeProviderAdm201 = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrDFeException, ACBrUtil.Base, ACBrUtil.Strings,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  Adm.GravarXml, Adm.LerXml;

{ TACBrNFSeXWebserviceAdm201 }

function TACBrNFSeXWebserviceAdm201.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := 'xmlns:nfse="' + TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.NameSpace + '"'
  else
    Result := 'xmlns:nfse="' + TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.NameSpace + '"';
end;

function TACBrNFSeXWebserviceAdm201.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('', Request,
                   ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincronoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar('', Request,
   ['RecepcionarLoteRpsSincronoResult', 'EnviarLoteRpsSincronoResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar('', Request,
                  ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseRpsRequest>';

  Result := Executar('', Request,
            ['ConsultarNfsePorRpsResult', 'ConsultarNfsePorRpsResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixaRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';

  Result := Executar('', Request,
        ['ConsultarNfsePorFaixaResult', 'ConsultarNfsePorFaixaResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar('', Request,
   ['ConsultarNfseServicoPrestadoResult', 'ConsultarNfseServicoPrestadoResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar('', Request,
   ['ConsultarNfseServicoTomadoResult', 'ConsultarNfseServicoTomadoResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.GerarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar('', Request,
                     ['GerarNfseResult', 'GerarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar('', Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceAdm201.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar('', Request,
                     ['SubstituirNfseResult', 'SubstituirNfseResposta'], []);
end;

{ TACBrNFSeProviderAdm201 }

procedure TACBrNFSeProviderAdm201.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ModoEnvio := meLoteAssincrono;

  ConfigGeral.Autenticacao.RequerLogin := True;
  ConfigGeral.Autenticacao.RequerChaveAcesso := True;
  ConfigGeral.Autenticacao.RequerChaveAutorizacao := True;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '1.00';
  end;

  SetXmlNameSpace('http://www.admnotafiscal.com.br/manual/nfse_v201.xsd');

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v201.xsd');
end;

function TACBrNFSeProviderAdm201.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Adm201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAdm201.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Adm201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAdm201.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAdm201.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderAdm201.ValidarSchema(Response: TNFSeWebserviceResponse;
  aMetodo: TMetodo);
var
  AErro: TNFSeEventoCollectionItem;
  xDados: string;
  i: Integer;
  aXml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  with TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente do
  begin
    if EstaVazio(WSUser) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod119;
      AErro.Descricao := ACBrStr(Desc119);
      Exit;
    end;

    if EstaVazio(WSChaveAcesso) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod124;
      AErro.Descricao := ACBrStr(Desc124);
      Exit;
    end;

    if EstaVazio(WSChaveAutoriz) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod125;
      AErro.Descricao := ACBrStr(Desc125);
      Exit;
    end;

    xDados := '<Key>' + WSChaveAcesso + '</Key>' +
              '<Auth>' + WSChaveAutoriz + '</Auth>' +
              '<RequestId>' + WSUser + '</RequestId>';
  end;

  aXml := Response.ArquivoEnvio;

  case aMetodo of
    tmRecepcionar,
    tmRecepcionarSincrono:
      begin
        i := Pos('<QuantidadeRps>', aXml);

        if i > 0 then
          aXml := Copy(aXml, 1, i-1) + xDados + Copy(aXml, i, Length(aXml));

        i := Pos('<ListaRps>', aXml);

        if i > 0 then
          aXml := Copy(aXml, 1, i-1) + '<Resposta>1</Resposta>' +
                    Copy(aXml, i, Length(aXml));

        Response.ArquivoEnvio := aXml;
      end;

    tmConsultarLote,
    tmConsultarNFSePorRps,
    tmConsultarNFSePorFaixa,
    tmConsultarNFSeServicoPrestado,
    tmConsultarNFSeServicoTomado:
      begin
        i := Pos('</Prestador>', aXml);

        if i > 0 then
          aXml := Copy(aXml, 1, i-1) + xDados + Copy(aXml, i, Length(aXml));

        Response.ArquivoEnvio := aXml;
      end;

    tmCancelarNFSe:
      begin
        i := Pos('<CodigoMunicipio>', aXml);

        if i > 0 then
          aXml := Copy(aXml, 1, i-1) + xDados + Copy(aXml, i, Length(aXml));

        Response.ArquivoEnvio := aXml;
      end;

    {
    tmGerar: ;
    tmSubstituirNFSe: ;
    }
  else
    Response.ArquivoEnvio := aXml;
  end;
end;

end.
