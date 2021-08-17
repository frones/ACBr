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

unit EloTech.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceEloTech = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;
  end;

  TACBrNFSeProviderEloTech = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function GerarRequerente(const CNPJ: string; const InscMunc: string;
      const Senha: string): string;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, EloTech.GravarXml, EloTech.LerXml;

{ TACBrNFSeProviderEloTech }

procedure TACBrNFSeProviderEloTech.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    Identificador := '';
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  SetXmlNameSpace('http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd');

  SetNomeXSD('nfse_v2_03.xsd');
end;

function TACBrNFSeProviderEloTech.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_EloTech.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_EloTech.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeProviderEloTech.GerarRequerente(const CNPJ, InscMunc,
  Senha: string): string;
var
  Homologacao: Boolean;
begin
  Homologacao := (TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 2);

  Result := '<IdentificacaoRequerente>' +
              '<CpfCnpj>' +
                '<Cnpj>' + CNPJ + '</Cnpj>' +
              '</CpfCnpj>' +
              '<InscricaoMunicipal>' + InscMunc + '</InscricaoMunicipal>' +
              '<Senha>' + Senha + '</Senha>' +
              '<Homologa>' +
                 LowerCase(booltostr(Homologacao, True)) +
              '</Homologa>' +
            '</IdentificacaoRequerente>';
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente, Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono] then
    begin
      Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

      Prestador := '<' + Prefixo2 + 'CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                   '</' + Prefixo2 + 'CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun, Prefixo2);

      Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                             Requerente +
                             '<' + Prefixo + 'LoteRps' + NameSpace2 + IdAttr  + Versao + '>' +
                               '<' + Prefixo2 + 'NumeroLote>' +
                                  Response.Lote +
                               '</' + Prefixo2 + 'NumeroLote>' +
                                 Prestador +
                               '<' + Prefixo2 + 'QuantidadeRps>' +
                                  IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                               '</' + Prefixo2 + 'QuantidadeRps>' +
                               '<' + Prefixo2 + 'ListaRps>' +
                                  Xml +
                               '</' + Prefixo2 + 'ListaRps>' +
                             '</' + Prefixo + 'LoteRps>' +
                           '</' + Prefixo + TagEnvio + '>';
    end
    else
      Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                              Xml +
                           '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente, NumeroLote: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.Lote <> '' then
      NumeroLote := '<' + Prefixo + 'NumeroLote>' +
                      Response.Lote +
                    '</' + Prefixo + 'NumeroLote>';

    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Requerente +
                           NumeroLote +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'IdentificacaoRps>' +
                             '<' + Prefixo2 + 'Numero>' +
                               Response.NumRPS +
                             '</' + Prefixo2 + 'Numero>' +
                             '<' + Prefixo2 + 'Serie>' +
                               Response.Serie +
                             '</' + Prefixo2 + 'Serie>' +
                             '<' + Prefixo2 + 'Tipo>' +
                               Response.Tipo +
                             '</' + Prefixo2 + 'Tipo>' +
                           '</' + Prefixo + 'IdentificacaoRps>' +
                           Requerente +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseFaixaEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoTomadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  Requerente, ChavedeAcesso: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    ChavedeAcesso := '<ChaveAcesso>' +
                        Trim(InfoCanc.CodVerificacao) +
                     '</ChaveAcesso>';

    Response.XmlEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo2 + 'Pedido>' +
                             '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + NameSpace2 + '>' +
                               '<' + Prefixo2 + 'IdentificacaoNfse>' +
                                 '<' + Prefixo2 + 'Numero>' +
                                    InfoCanc.NumeroNFSe +
                                 '</' + Prefixo2 + 'Numero>' +
                                 Serie +
                                 '<' + Prefixo2 + 'CpfCnpj>' +
                                   GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                                 '</' + Prefixo2 + 'CpfCnpj>' +
                                 GetInscMunic(Emitente.InscMun, Prefixo2) +
                                 '<' + Prefixo2 + 'CodigoMunicipio>' +
                                    IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                                 '</' + Prefixo2 + 'CodigoMunicipio>' +
                               '</' + Prefixo2 + 'IdentificacaoNfse>' +
                               ChavedeAcesso +
                               '<' + Prefixo2 + 'CodigoCancelamento>' +
                                  InfoCanc.CodCancelamento +
                               '</' + Prefixo2 + 'CodigoCancelamento>' +
                               Motivo +
                             '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                           '</' + Prefixo2 + 'Pedido>' +
                         '</' + Prefixo + 'CancelarNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
    Response.PedCanc := RetornarConteudoEntre(Response.PedCanc,
                                                 '<Pedido>', '</Pedido>', True);

    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo + 'SubstituicaoNfse' + IdAttr + '>' +
                             Response.PedCanc +
                             Xml +
                           '</' + Prefixo + 'SubstituicaoNfse>' +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

{ TACBrNFSeXWebserviceEloTech }

function TACBrNFSeXWebserviceEloTech.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'EnviarLoteRpsResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'EnviarLoteRpsSincronoResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'ConsultarLoteRpsResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'ConsultarNfsePorFaixaResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'ConsultarNfseRpsResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'CancelarNfseResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG,
//                     ['return', 'outputXML', 'SubstituirNfseResposta'],
                     [''],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

end.
