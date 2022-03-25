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

{
  Só funcionou usando o HttpWinNet
}

{$I ACBr.inc}

unit ISSNet.Provider;

interface

uses
  SysUtils, Classes,
  ACBrConsts, ACBrXmlBase, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSNet = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderISSNet = class (TACBrNFSeProviderABRASFv1)
  protected
    FpNameSpaceCanc: string;

    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSNet.GravarXml, ISSNet.LerXml;

{ TACBrNFSeProviderISSNet }

procedure TACBrNFSeProviderISSNet.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';

    with TACBrNFSeX(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        CodIBGE := IntToStr(Configuracoes.Geral.CodigoMunicipio)
      else
        CodIBGE := '999';
    end;
  end;

  with ConfigMsgDados do
  begin
    PrefixoTS := 'tc';

    XmlRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd';

    LoteRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_enviar_lote_rps_envio.xsd';

    ConsultarSituacao.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_situacao_lote_rps_envio.xsd';

    ConsultarLote.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_lote_rps_envio.xsd';

    ConsultarNFSeRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_rps_envio.xsd';

    ConsultarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_envio.xsd';

    CancelarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd';
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio.xsd';
  end;

  FpNameSpaceCanc := ' xmlns:ts="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_simples.xsd"' +
                     ' xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"' +
                     ' xmlns:p1="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd"';
end;

function TACBrNFSeProviderISSNet.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSNet.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderISSNet.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    NameSpace := FpNameSpaceCanc;

    Response.ArquivoEnvio := '<Pedido' + NameSpace + '>' +
                           '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + '>' +
                             '<' + Prefixo2 + 'IdentificacaoNfse>' +
                               '<' + Prefixo2 + 'Numero>' +
                                 InfoCanc.NumeroNFSe +
                               '</' + Prefixo2 + 'Numero>' +
                               '<' + Prefixo2 + 'Cnpj>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</' + Prefixo2 + 'Cnpj>' +
                               GetInscMunic(Emitente.InscMun, Prefixo2) +
                               '<' + Prefixo2 + 'CodigoMunicipio>' +
                                  ConfigGeral.CodIBGE +
                               '</' + Prefixo2 + 'CodigoMunicipio>' +
                             '</' + Prefixo2 + 'IdentificacaoNfse>' +
                             '<' + Prefixo2 + 'CodigoCancelamento>' +
                                InfoCanc.CodCancelamento +
                             '</' + Prefixo2 + 'CodigoCancelamento>' +
                             Motivo +
                           '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                         '</Pedido>';
  end;
end;

procedure TACBrNFSeProviderISSNet.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
  i, j: Integer;
begin
  if aMetodo <> tmCancelarNFSe then
  begin
    xXml := Response.ArquivoEnvio;

    i := Pos('<tc:Cnpj>', xXml) -1;
    j := Pos('<tc:InscricaoMunicipal>', xXml);

    xXml := Copy(xXml, 1, i) +
            '<tc:CpfCnpj>' + Copy(xXml, i+1, j-i-1) + '</tc:CpfCnpj>' +
            Copy(xXml, j, Length(xXml));

    Response.ArquivoEnvio := xXml;

    inherited ValidarSchema(Response, aMetodo);
  end
  else
  begin
    xXml := Response.ArquivoEnvio;
    xXml := StringReplace(xXml, FpNameSpaceCanc, '', []);
    xXml := '<p1:CancelarNfseEnvio' + FpNameSpaceCanc + '>' +
              xXml +
            '</p1:CancelarNfseEnvio>';

    Response.ArquivoEnvio := xXml;
  end;
end;

{ TACBrNFSeXWebserviceISSNet }

function TACBrNFSeXWebserviceISSNet.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/RecepcionarLoteRps',
                     Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarLoteRps',
                     Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultaSituacaoLoteRPS xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultaSituacaoLoteRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultaSituacaoLoteRPS',
                     Request,
                     ['ConsultaSituacaoLoteRPSResult', 'ConsultarSituacaoLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNFSePorRPS xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarNFSePorRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNFSePorRPS',
                     Request,
                     ['ConsultarNFSePorRPSResult', 'ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfse xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</ConsultarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNfse',
                     Request,
                     ['ConsultarNfseResult', 'ConsultarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse xmlns="http://www.issnetonline.com.br/webservice/nfd">';
  Request := Request + '<xml>' + XmlToStr(CUTF8DeclaracaoXML + AMSG) + '</xml>';
  Request := Request + '</CancelarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/CancelarNfse',
                     Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceISSNet.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, False);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
