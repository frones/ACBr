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

unit ACBrNFSeXProviderBase;

interface

uses
  SysUtils, Classes,
  ACBrDFe,
  ACBrNFSeXParametros, ACBrNFSeXInterface, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXLerXml, ACBrNFSeXGravarXml, ACBrNFSeXNotasFiscais,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXProvider = class(TInterfacedObject, IACBrNFSeXProvider)
  private
    FConfigGeral: TConfigGeral;
    FConfigWebServices: TConfigWebServices;
    FConfigMsgDados: TConfigMsgDados;
    FConfigAssinar: TConfigAssinar;
    FConfigSchemas: TConfigSchemas;
    FDefaultNameSpaceURI: string;

    function GetConfigGeral: TConfigGeral;
    function GetConfigWebServices: TConfigWebServices;
    function GetConfigMsgDados: TConfigMsgDados;
    function GetConfigAssinar: TConfigAssinar;
    function GetConfigSchemas: TConfigSchemas;

    function GetGerarResponse: TNFSeEmiteResponse;
    function GetEmiteResponse: TNFSeEmiteResponse;
    function GetConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse;
    function GetConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse;
    function GetConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse;
    function GetConsultaNFSeResponse: TNFSeConsultaNFSeResponse;
    function GetConsultaLinkNFSeResponse: TNFSeConsultaLinkNFSeResponse;
    function GetCancelaNFSeResponse: TNFSeCancelaNFSeResponse;
    function GetSubstituiNFSeResponse: TNFSeSubstituiNFSeResponse;
    function GetGerarTokenResponse: TNFSeGerarTokenResponse;
    function GetEnviarEventoResponse: TNFSeEnviarEventoResponse;
    function GetConsultarEventoResponse: TNFSeConsultarEventoResponse;
    function GetConsultarDFeResponse: TNFSeConsultarDFeResponse;
    function GetConsultarParamResponse: TNFSeConsultarParamResponse;
    function GetConsultarSeqRpsResponse: TNFSeConsultarSeqRpsResponse;

  protected
    FAOwner: TACBrDFe;
    PrefixoTS: string;

    procedure Configuracao; virtual;
    procedure CarregarURL; virtual;
    procedure SetNomeXSD(const aNome: string);
    procedure SetXmlNameSpace(const aNameSpace: string);
    procedure SetNameSpaceURI(const aMetodo: TMetodo);
    procedure SalvarXmlRps(aNota: TNotaFiscal);
    procedure SalvarXmlNfse(aNota: TNotaFiscal);
    procedure SalvarPDFNfse(const aNome: string; const aPDF: AnsiString);
    procedure SalvarXmlEvento(const aNome: string; const  aEvento: AnsiString);

    function CarregarXmlNfse(aNota: TNotaFiscal; aXml: string): TNotaFiscal;

    function GetWebServiceURL(const AMetodo: TMetodo): string;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; virtual; abstract;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; virtual; abstract;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; virtual; abstract;
    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); virtual;

    function PrepararRpsParaLote(const aXml: string): string; virtual;
    function GetCpfCnpj(const CpfCnpj: string; const Prefixo: string = ''): string;
    function GetInscMunic(const InscMunic: string; const Prefixo: string = ''): string;
    function GetCabecalho(const Xmlns: string = ''): string;
    function DefinirIDLote(const ID: string): string; virtual;
    function DefinirIDCancelamento(const CNPJ: string; const InscMunic: string;
                                   const NumNfse: string): string; virtual;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); virtual; abstract;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarEmitir(Response: TNFSeEmiteResponse); virtual;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaSituacao
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaSituacao(Response: TNFSeConsultaSituacaoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaNFSe(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaLinkNFSe
    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo SubstituirNFSe
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo GerarToken
    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); virtual; abstract;
    procedure GerarMsgDadosGerarToken(Response: TNFSeGerarTokenResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarGerarToken(Response: TNFSeGerarTokenResponse); virtual;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo EnviarEvento
    procedure PrepararEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual; abstract;
    procedure GerarMsgDadosEnviarEvento(Response: TNFSeEnviarEventoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual;
    procedure TratarRetornoEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultarEvento
    procedure PrepararConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarEvento(Response: TNFSeConsultarEventoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual;
    procedure TratarRetornoConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultarDFe
    procedure PrepararConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarDFe(Response: TNFSeConsultarDFeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual;
    procedure TratarRetornoConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultarParam
    procedure PrepararConsultarParam(Response: TNFSeConsultarParamResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarParam(Response: TNFSeConsultarParamResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarParam(Response: TNFSeConsultarParamResponse); virtual;
    procedure TratarRetornoConsultarParam(Response: TNFSeConsultarParamResponse); virtual; abstract;

    //método usado para consultar o último rps convertido em nfse
    procedure PrepararConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); virtual; abstract;
    procedure AssinarConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); virtual;
    procedure TratarRetornoConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); virtual; abstract;

  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    function GetSchemaPath: string; virtual;

    function GerarXml(const aNFSe: TNFSe; var aXml, aAlerts: string): Boolean; virtual;
    function LerXML(const aXML: String; var aNFSe: TNFSe; var ATipo: TtpXML;
      var aXmlTratado: string): Boolean; virtual;

    procedure GeraLote; virtual;
    procedure Emite; virtual;
    procedure ConsultaSituacao; virtual;
    procedure ConsultaLoteRps; virtual;
    procedure ConsultaNFSeporRps; virtual;
    procedure ConsultaNFSe; virtual;
    procedure ConsultaLinkNFSe; virtual;
    procedure CancelaNFSe; virtual;
    procedure SubstituiNFSe; virtual;
    procedure GerarToken; virtual;
    procedure EnviarEvento; virtual;
    procedure ConsultarEvento; virtual;
    procedure ConsultarDFe; virtual;
    procedure ConsultarParam; virtual;
    procedure ConsultarSeqRps; virtual;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;

    property GerarResponse: TNFSeEmiteResponse read GetGerarResponse;
    property EmiteResponse: TNFSeEmiteResponse read GetEmiteResponse;
    property ConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse read GetConsultaSituacaoResponse;
    property ConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse read GetConsultaLoteRpsResponse;
    property ConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse read GetConsultaNFSeporRpsResponse;
    property ConsultaNFSeResponse: TNFSeConsultaNFSeResponse read GetConsultaNFSeResponse;
    property ConsultaLinkNFSeResponse: TNFSeConsultaLinkNFSeResponse read GetConsultaLinkNFSeResponse;
    property CancelaNFSeResponse: TNFSeCancelaNFSeResponse read GetCancelaNFSeResponse;
    property SubstituiNFSeResponse: TNFSeSubstituiNFSeResponse read GetSubstituiNFSeResponse;
    property GerarTokenResponse: TNFSeGerarTokenResponse read GetGerarTokenResponse;
    property EnviarEventoResponse: TNFSeEnviarEventoResponse read GetEnviarEventoResponse;
    property ConsultarEventoResponse: TNFSeConsultarEventoResponse read GetConsultarEventoResponse;
    property ConsultarDFeResponse: TNFSeConsultarDFeResponse read GetConsultarDFeResponse;
    property ConsultarParamResponse: TNFSeConsultarParamResponse read GetConsultarParamResponse;
    property ConsultarSeqRpsResponse: TNFSeConsultarSeqRpsResponse read GetConsultarSeqRpsResponse;

    function SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string; virtual;
    function StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps; virtual;
    function SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string; virtual;

    function SimNaoToStr(const t: TnfseSimNao): string; virtual;
    function StrToSimNao(out ok: boolean; const s: string): TnfseSimNao; virtual;
    function SimNaoDescricao(const t: TnfseSimNao): string; virtual;

    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; virtual;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; virtual;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; virtual;

    function SituacaoTributariaToStr(const t: TnfseSituacaoTributaria): string; virtual;
    function StrToSituacaoTributaria(out ok: boolean; const s: string): TnfseSituacaoTributaria; virtual;
    function SituacaoTributariaDescricao(const t: TnfseSituacaoTributaria): string; virtual;

    function ResponsavelRetencaoToStr(const t: TnfseResponsavelRetencao): string; virtual;
    function StrToResponsavelRetencao(out ok: boolean; const s: string): TnfseResponsavelRetencao; virtual;
    function ResponsavelRetencaoDescricao(const t: TnfseResponsavelRetencao): String; virtual;

    function NaturezaOperacaoDescricao(const t: TnfseNaturezaOperacao): string; virtual;

    function TipoPessoaToStr(const t: TTipoPessoa): string; virtual;
    function StrToTipoPessoa(out ok: boolean; const s: string): TTipoPessoa; virtual;

    function ExigibilidadeISSToStr(const t: TnfseExigibilidadeISS): string; virtual;
    function StrToExigibilidadeISS(out ok: boolean; const s: string): TnfseExigibilidadeISS; virtual;
    function ExigibilidadeISSDescricao(const t: TnfseExigibilidadeISS): string; virtual;

    function TipoRPSToStr(const t:TTipoRPS): string; virtual;
    function StrToTipoRPS(out ok: boolean; const s: string): TTipoRPS; virtual;

    function SituacaoTribToStr(const t: TSituacaoTrib): string; virtual;
    function StrToSituacaoTrib(out ok: boolean; const s: string): TSituacaoTrib; virtual;

    function TributacaoToStr(const t: TTributacao): string; virtual;
    function StrToTributacao(out ok: boolean; const s: string): TTributacao; virtual;
    function TributacaoDescricao(const t: TTributacao): String; virtual;

    function TipoDeducaoToStr(const t: TTipoDeducao): string; virtual;
    function StrToTipoDeducao(out ok: Boolean; const s: string): TTipoDeducao; virtual;

    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string; virtual;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS; virtual;

    function CondicaoPagToStr(const t: TnfseCondicaoPagamento): string; virtual;
    function StrToCondicaoPag(out ok: boolean; const s: string): TnfseCondicaoPagamento; virtual;
  end;

implementation

uses
  IniFiles,
  pcnAuxiliar,
  ACBrConsts,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrXmlBase, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts;

{
  Ainda não é possível remover o pcnAuxiliar, pois utiliza o:
  TimeZoneConf.Assign.
}

{ TACBrNFSeXProvider }

constructor TACBrNFSeXProvider.Create(AOwner: TACBrDFe);
begin
  inherited Create;

  FAOwner := AOwner;
  if not Assigned(FAOwner) then
    raise EACBrDFeException.Create('Componente ACBrNFSeX não informado');

  FConfigGeral := TConfigGeral.Create;
  FConfigWebServices := TConfigWebServices.Create;
  FConfigMsgDados := TConfigMsgDados.Create;
  FConfigAssinar := TConfigAssinar.Create;
  FConfigSchemas := TConfigSchemas.Create;

  Configuracao;
end;

function TACBrNFSeXProvider.DefinirIDCancelamento(const CNPJ: string;
  const InscMunic: string; const NumNfse: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="Canc_' + CNPJ + InscMunic +
              NumNfse + '"'
  else
    Result := '';
end;

function TACBrNFSeXProvider.DefinirIDLote(const ID: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="Lote_' + ID + '"'
  else
    Result := '';
end;

destructor TACBrNFSeXProvider.Destroy;
begin
  FConfigGeral.Free;
  FConfigWebServices.Free;
  FConfigMsgDados.Free;
  FConfigAssinar.Free;
  FConfigSchemas.Free;

  inherited Destroy;
end;

function TACBrNFSeXProvider.GetConfigGeral: TConfigGeral;
begin
  Result := FConfigGeral;
end;

function TACBrNFSeXProvider.GetConfigWebServices: TConfigWebServices;
begin
  Result := FConfigWebServices;
end;

function TACBrNFSeXProvider.GetGerarResponse: TNFSeEmiteResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.Gerar;
end;

function TACBrNFSeXProvider.GetEmiteResponse: TNFSeEmiteResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.Emite;
end;

function TACBrNFSeXProvider.GetConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaSituacao;
end;

function TACBrNFSeXProvider.GetConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaLoteRps;
end;

function TACBrNFSeXProvider.GetConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaNFSeporRps;
end;

function TACBrNFSeXProvider.GetConsultaNFSeResponse: TNFSeConsultaNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaNFSe;
end;

function TACBrNFSeXProvider.GetConsultarEventoResponse: TNFSeConsultarEventoResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultarEvento;
end;

function TACBrNFSeXProvider.GetConsultarParamResponse: TNFSeConsultarParamResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultarParam;
end;

function TACBrNFSeXProvider.GetConsultarDFeResponse: TNFSeConsultarDFeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultarDFe;
end;

function TACBrNFSeXProvider.GetConsultaLinkNFSeResponse: TNFSeConsultaLinkNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaLinkNFSe;
end;

function TACBrNFSeXProvider.GetCancelaNFSeResponse: TNFSeCancelaNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.CancelaNFSe;
end;

function TACBrNFSeXProvider.GetSubstituiNFSeResponse: TNFSeSubstituiNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.SubstituiNFSe;
end;

function TACBrNFSeXProvider.GetGerarTokenResponse: TNFSeGerarTokenResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.GerarToken;
end;

function TACBrNFSeXProvider.GetEnviarEventoResponse: TNFSeEnviarEventoResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.EnviarEvento;
end;

function TACBrNFSeXProvider.GetConsultarSeqRpsResponse: TNFSeConsultarSeqRpsResponse;
begin
  Result := TACBrNFSeX(FAOwner).WebService.ConsultarSeqRps;
end;

function TACBrNFSeXProvider.GetSchemaPath: string;
begin
  with TACBrNFSeX(FAOwner).Configuracoes do
  begin
    Result := PathWithDelim(Arquivos.PathSchemas + Geral.xProvedor);
    Result := PathWithDelim(Result + VersaoNFSeToStr(Geral.Versao));
  end;
end;

function TACBrNFSeXProvider.GetCpfCnpj(const CpfCnpj: string; const Prefixo: string): string;
var
  xCpfCnpj: string;
begin
  xCpfCnpj := OnlyNumber(CpfCnpj);

  if xCpfCnpj <> '' then
  begin
    if Length(xCpfCnpj) = 14 then
      Result := '<' + Prefixo + 'Cnpj>' + xCpfCnpj + '</' + Prefixo + 'Cnpj>'
    else
      Result := '<' + Prefixo + 'Cpf>' + xCpfCnpj + '</' + Prefixo + 'Cpf>';
  end
  else
    Result := '';
end;

function TACBrNFSeXProvider.GetInscMunic(const InscMunic: string; const Prefixo: string): string;
begin
  if NaoEstaVazio(InscMunic) then
    Result := '<' + Prefixo + 'InscricaoMunicipal>' + InscMunic +
              '</' + Prefixo + 'InscricaoMunicipal>'
  else
    Result := '';
end;

function TACBrNFSeXProvider.GetWebServiceURL(const AMetodo: TMetodo): string;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        // Métodos padrões da versão 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // Métodos padrões da versão 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // Métodos que por padrão não existem na versão 1 e 2 do layout da ABRASF
        tmGerarToken: Result := GerarToken;
        tmAbrirSessao: Result := AbrirSessao;
        tmFecharSessao: Result := FecharSessao;
        tmTeste: Result := TesteEnvio;
        tmEnviarEvento: Result := EnviarEvento;
        tmConsultarEvento: Result := ConsultarEvento;
        tmConsultarDFe: Result := ConsultarDFe;
        tmConsultarParam: Result := ConsultarParam;
        tmConsultarSeqRps: Result := ConsultarSeqRps;
        tmConsultarLinkNFSe: Result := ConsultarLinkNFSe;
        tmConsultarNFSePorChave: Result := ConsultarNFSePorChave;
      else
        Result := '';
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        // Métodos padrões da versão 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // Métodos padrões da versão 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // Métodos que por padrão não existem na versão 1 e 2 do layout da ABRASF
        tmGerarToken: Result := GerarToken;
        tmAbrirSessao: Result := AbrirSessao;
        tmFecharSessao: Result := FecharSessao;
        tmTeste: Result := TesteEnvio;
        tmEnviarEvento: Result := EnviarEvento;
        tmConsultarEvento: Result := ConsultarEvento;
        tmConsultarDFe: Result := ConsultarDFe;
        tmConsultarParam: Result := ConsultarParam;
        tmConsultarSeqRps: Result := ConsultarSeqRps;
        tmConsultarLinkNFSe: Result := ConsultarLinkNFSe;
        tmConsultarNFSePorChave: Result := ConsultarNFSePorChave;
      else
        Result := '';
      end;
    end;
  end;
end;

function TACBrNFSeXProvider.GetConfigMsgDados: TConfigMsgDados;
begin
  Result := FConfigMsgDados;
end;

function TACBrNFSeXProvider.GetCabecalho(const Xmlns: string): string;
var
  Versao, NameSpace: string;
begin
  Versao := ' versao="' + ConfigWebServices.VersaoAtrib + '"';

  NameSpace := Xmlns;

  if NameSpace = '' then
    NameSpace := ConfigMsgDados.XmlRps.xmlns;

  NameSpace := ' xmlns="' + NameSpace + '"';

  Result := '<cabecalho' + Versao + NameSpace + '>' +
            '<versaoDados>' + ConfigWebServices.VersaoDados + '</versaoDados>' +
            '</cabecalho>';
end;

function TACBrNFSeXProvider.GetConfigAssinar: TConfigAssinar;
begin
  Result := FConfigAssinar;
end;

function TACBrNFSeXProvider.GetConfigSchemas: TConfigSchemas;
begin
  Result := FConfigSchemas;
end;

procedure TACBrNFSeXProvider.Configuracao;
begin
  // Inicializa os parâmetros de configuração: Geral
  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;
    NumMaxRpsGerar  := 1;
    NumMaxRpsEnviar := 50;
    NumMinRpsEnviar := 1;
    ModoEnvio := meAutomatico;
    TabServicosExt := False;
    Identificador := 'Id';
    ConsultaSitLote := False;
    ConsultaLote := True;
    ConsultaNFSe := True;
    QuebradeLinha := ';';
    CancPreencherMotivo := False;
    CancPreencherSerieNfse := False;
    CancPreencherCodVerificacao := False;
    DetalharServico := False;
    FormatoArqEnvio := tfaXml;
    FormatoArqRetorno := tfaXml;
    FormatoArqEnvioSoap := tfaXml;
    FormatoArqRetornoSoap := tfaXml;
    FormatoArqRecibo := tfaXml;
    FormatoArqNota := tfaXml;
    FormatoArqEvento := tfaXml;

    with Autenticacao do
    begin
      RequerCertificado := True;
      RequerLogin := False;
      RequerChaveAcesso := False;
      RequerChaveAutorizacao := False;
      RequerFraseSecreta := False;
    end;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := False;
      EnviarLoteSincrono := False;
      EnviarUnitario := False;
      ConsultarSituacao := False;
      ConsultarLote := False;
      ConsultarRps := False;
      ConsultarNfse := False;
      ConsultarFaixaNfse := False;
      ConsultarServicoPrestado := False;
      ConsultarServicoTomado := False;
      CancelarNfse := False;
      SubstituirNfse := False;
      GerarToken := False;
      EnviarEvento := False;
      ConsultarEvento := False;
      ConsultarDFe := False;
      ConsultarParam := False;
      ConsultarSeqRps := False;
      ConsultarLinkNfse := False;
      ConsultarNfseChave := False;
      TestarEnvio := False;
    end;

    with TACBrNFSeX(FAOwner) do
    begin
      Provedor := Configuracoes.Geral.Provedor;
      Versao := Configuracoes.Geral.Versao;
      xMunicipio := Configuracoes.Geral.xMunicipio;

      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        Ambiente := taProducao
      else
        Ambiente := taHomologacao;

      CodIBGE := IntToStr(Configuracoes.Geral.CodigoMunicipio);
      IniTabServicos := Configuracoes.Arquivos.IniTabServicos;
    end;
  end;

  // Inicializa os parâmetros de configuração: MsgDados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';
    PrefixoTS := '';

    UsarNumLoteConsLote := False;

    // Usado para geração do Xml do Rps
    with XmlRps do
    begin
      xmlns := '';
      InfElemento := 'InfRps';
      DocElemento := 'Rps';
    end;

    // Usado para geração do Envio do Lote em modo assíncrono
    with LoteRps do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsEnvio';
    end;

    // Usado para geração do Envio do Lote em modo Sincrono
    with LoteRpsSincrono do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsSincronoEnvio';
    end;

    // Usado para geração da Consulta a Situação do Lote
    with ConsultarSituacao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarSituacaoLoteRpsEnvio';
    end;

    // Usado para geração da Consulta do Lote
    with ConsultarLote do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarLoteRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe por RPS
    with ConsultarNFSeRps do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe
    with ConsultarNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseEnvio';
    end;

    // Usado para geração da Consulta da NFSe Por Chave
    with ConsultarNFSePorChave do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração da Consulta da NFSe Por Faixa
    with ConsultarNFSePorFaixa do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseFaixaEnvio';
    end;

    // Usado para geração da Consulta da NFSe Servico Prestado
    with ConsultarNFSeServicoPrestado do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseServicoPrestadoEnvio';
    end;

    // Usado para geração da Consulta da NFSe Servico Tomado
    with ConsultarNFSeServicoTomado do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseServicoTomadoEnvio';
    end;

    // Usado para geração do Cancelamento
    with CancelarNFSe do
    begin
      xmlns := '';
      InfElemento := 'InfPedidoCancelamento';
      DocElemento := 'Pedido';
    end;

    // Usado para geração do Gerar
    with GerarNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'GerarNfseEnvio';
    end;

    // Usado para geração do Substituir
    with SubstituirNFSe do
    begin
      xmlns := '';
      InfElemento := 'SubstituicaoNfse';
      DocElemento := 'SubstituirNfseEnvio';
    end;

    // Usado para geração da Abertura de Sessão
    with AbrirSessao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Fechamento de Sessão
    with FecharSessao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Gerar Token
    with GerarToken do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Enviar Evento
    with EnviarEvento do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Consultar Evento
    with ConsultarEvento do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Consultar Evento
    with ConsultarDFe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Consultar Parâmetros
    with ConsultarParam do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    with ConsultarSeqRps do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    with ConsultarLinkNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;
  end;

  // Inicializa os parâmetros de configuração: Assinar
  with ConfigAssinar do
  begin
    Rps := False;
    LoteRps := False;
    ConsultarSituacao := False;
    ConsultarLote := False;
    ConsultarNFSeRps := False;
    ConsultarNFSe := False;
    ConsultarNFSePorChave := False;
    ConsultarNFSePorFaixa := False;
    ConsultarNFSeServicoPrestado := False;
    ConsultarNFSeServicoTomado := False;
    CancelarNFSe := False;
    RpsGerarNFSe := False;
    LoteGerarNFSe := False;
    RpsSubstituirNFSe := False;
    SubstituirNFSe := False;
    AbrirSessao := False;
    FecharSessao := False;
    GerarToken := False;
    EnviarEvento := False;
    ConsultarEvento := False;
    ConsultarDFe := False;
    ConsultarParam := False;
    ConsultarSeqRps := False;
    ConsultarLinkNFSe := False;

    IncluirURI := True;

    AssinaturaAdicional := False;
    Assinaturas := TACBrNFSeX(FAOwner).Configuracoes.Geral.Assinaturas;
  end;

  SetNomeXSD('nfse.xsd');

  ConfigWebServices.AtribVerLote := '';

  CarregarURL;
end;

procedure TACBrNFSeXProvider.CarregarURL;
var
  IniParams: TMemIniFile;
  Sessao: String;
begin
  IniParams := TMemIniFile.Create('');

  with TACBrNFSeX(FAOwner) do
  begin
    IniParams.SetStrings(Configuracoes.WebServices.Params);
  end;

  try
    with TACBrNFSeX(FAOwner) do
    begin
      // Primeiro verifica as URLs definidas para a cidade
      Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
      ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadLinkUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadLinkUrlHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadXMLNameSpaceProducao(IniParams, Sessao);
      ConfigWebServices.LoadXMLNameSpaceHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadNameSpaceProducao(IniParams, Sessao);
      ConfigWebServices.LoadNameSpaceHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadSoapActionProducao(IniParams, Sessao);
      ConfigWebServices.LoadSoapActionHomologacao(IniParams, Sessao);

      ConfigGeral.LoadParams(IniParams, Sessao);

      // Depois verifica as URLs definidas para o provedor
      if (ConfigWebServices.Producao.Recepcionar = '') or
         (Configuracoes.Geral.Provedor = proPadraoNacional) then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      end;

      if (ConfigWebServices.Homologacao.Recepcionar = '') or
         (Configuracoes.Geral.Provedor = proPadraoNacional) then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      end;

      if (ConfigWebServices.Producao.LinkURL = '') or
         (Configuracoes.Geral.Provedor = proPadraoNacional) then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadlinkUrlProducao(IniParams, Sessao);
      end;

      if (ConfigWebServices.Homologacao.LinkURL = '') or
         (Configuracoes.Geral.Provedor = proPadraoNacional) then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadLinkUrlHomologacao(IniParams, Sessao);
      end;

      if ConfigWebServices.Producao.XMLNameSpace = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadXMLNameSpaceProducao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.XMLNameSpace = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadXMLNameSpaceHomologacao(IniParams, Sessao);
      end;

      if ConfigWebServices.Producao.NameSpace = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadNameSpaceProducao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.NameSpace = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadNameSpaceHomologacao(IniParams, Sessao);
      end;

      if ConfigWebServices.Producao.SoapAction = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadSoapActionProducao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.SoapAction = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadSoapActionHomologacao(IniParams, Sessao);
      end;

      // Se Params1 estiver vazio usar o que foi definido para o provedor
      if (ConfigGeral.Params.AsString = '') then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigGeral.LoadParams(IniParams, Sessao);
      end;
    end;
  finally
    IniParams.Free;
  end;
end;

function TACBrNFSeXProvider.CarregarXmlNfse(aNota: TNotaFiscal; aXml: string): TNotaFiscal;
begin
  if Assigned(ANota) then
    ANota.XmlNfse := aXml
  else
  begin
    TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(aXml, False);
    ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
  end;

  Result := aNota;
end;

procedure TACBrNFSeXProvider.SalvarXmlRps(aNota: TNotaFiscal);
var
  Extensao: string;
  ConteudoEhXml: Boolean;
begin
  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    case ConfigGeral.FormatoArqRecibo of
      tfaJson:
        Extensao := '.json';
      tfaTxt:
        Extensao := '.txt';
    else
      Extensao := '.xml';
    end;

    if ConfigGeral.FormatoArqRecibo <> tfaXml then
    begin
      aNota.XmlRps := RemoverDeclaracaoXML(aNota.XmlRps);
      ConteudoEhXml := False;
    end
    else
      ConteudoEhXml := True;

    if EstaVazio(aNota.NomeArqRps) then
      aNota.NomeArqRps := aNota.CalcularNomeArquivoCompleto(aNota.NomeArqRps, '');

    if not ConteudoEhXml then
      aNota.NomeArqRps := StringReplace(aNota.NomeArqRps, '.xml', Extensao, [rfReplaceAll]);

    TACBrNFSeX(FAOwner).Gravar(aNota.NomeArqRps, aNota.XmlRps, '', ConteudoEhXml);
  end;
end;

procedure TACBrNFSeXProvider.SalvarXmlNfse(aNota: TNotaFiscal);
var
  aPath, aNomeArq, Extensao: string;
  aConfig: TConfiguracoesNFSe;
  ConteudoEhXml: Boolean;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathNFSe(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

  aNomeArq := TACBrNFSeX(FAOwner).GetNumID(aNota.NFSe) + '-nfse.xml';
  aNota.NomeArq := PathWithDelim(aPath) + aNomeArq;
  aNota.Confirmada := True;

  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    case ConfigGeral.FormatoArqNota of
      tfaJson:
        Extensao := '.json';
      tfaTxt:
        Extensao := '.txt';
    else
      Extensao := '.xml';
    end;

    if ConfigGeral.FormatoArqNota <> tfaXml then
    begin
      aNota.XmlNfse := RemoverDeclaracaoXML(aNota.XmlNfse);
      ConteudoEhXml := False;
    end
    else
      ConteudoEhXml := True;

    if not ConteudoEhXml then
      aNota.NomeArq := StringReplace(aNota.NomeArq, '.xml', Extensao, [rfReplaceAll]);

    TACBrNFSeX(FAOwner).Gravar(aNota.NomeArq, aNota.XmlNfse, '', ConteudoEhXml);
  end;
end;

procedure TACBrNFSeXProvider.SalvarPDFNfse(const aNome: string;
  const aPDF: AnsiString);
var
  aPath, aNomeArq: string;
  aConfig: TConfiguracoesNFSe;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathNFSe(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

  aNomeArq := PathWithDelim(aPath) + aNome + '-nfse.pdf';

  WriteToTXT(aNomeArq, aPDF, False, False);
end;

procedure TACBrNFSeXProvider.SalvarXmlEvento(const aNome: string;
  const  aEvento: AnsiString);
var
  aPath, aNomeArq, Extensao: string;
  aConfig: TConfiguracoesNFSe;
  ConteudoEhXml: Boolean;
  ArqEvento: AnsiString;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathEvento(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

  aNomeArq := PathWithDelim(aPath) + aNome + '.xml';

  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    case ConfigGeral.FormatoArqEvento of
      tfaJson:
        Extensao := '.json';
      tfaTxt:
        Extensao := '.txt';
    else
      Extensao := '.xml';
    end;

    if ConfigGeral.FormatoArqEvento <> tfaXml then
    begin
      ArqEvento := RemoverDeclaracaoXML(aEvento);
      ConteudoEhXml := False;
    end
    else
    begin
      ArqEvento := aEvento;
      ConteudoEhXml := True;
    end;

    if not ConteudoEhXml then
      aNomeArq := StringReplace(aNomeArq, '.xml', Extensao, [rfReplaceAll]);

    WriteToTXT(aNomeArq, ArqEvento, False, False);
  end;
end;

procedure TACBrNFSeXProvider.SetNameSpaceURI(const aMetodo: TMetodo);
var
  xNameSpaceURI: String;
begin
  case aMetodo of
    tmRecepcionar: xNameSpaceURI := ConfigMsgDados.LoteRps.xmlns;
    tmRecepcionarSincrono: xNameSpaceURI := ConfigMsgDados.LoteRpsSincrono.xmlns;
    tmConsultarSituacao: xNameSpaceURI := ConfigMsgDados.ConsultarSituacao.xmlns;
    tmConsultarLote: xNameSpaceURI := ConfigMsgDados.ConsultarLote.xmlns;
    tmConsultarNFSePorRps: xNameSpaceURI := ConfigMsgDados.ConsultarNFSeRps.xmlns;
    tmConsultarNFSe: xNameSpaceURI := ConfigMsgDados.ConsultarNFSe.xmlns;
    tmConsultarNFSePorChave: xNameSpaceURI := ConfigMsgDados.ConsultarNFSePorChave.xmlns;
    tmConsultarNFSePorFaixa: xNameSpaceURI := ConfigMsgDados.ConsultarNFSePorFaixa.xmlns;
    tmConsultarNFSeServicoPrestado: xNameSpaceURI := ConfigMsgDados.ConsultarNFSeServicoPrestado.xmlns;
    tmConsultarNFSeServicoTomado: xNameSpaceURI := ConfigMsgDados.ConsultarNFSeServicoTomado.xmlns;
    tmCancelarNFSe: xNameSpaceURI := ConfigMsgDados.CancelarNFSe.xmlns;
    tmGerar: xNameSpaceURI := ConfigMsgDados.GerarNFSe.xmlns;
    tmSubstituirNFSe: xNameSpaceURI := ConfigMsgDados.SubstituirNFSe.xmlns;
    tmAbrirSessao: xNameSpaceURI := ConfigMsgDados.AbrirSessao.xmlns;
    tmFecharSessao: xNameSpaceURI := ConfigMsgDados.FecharSessao.xmlns;
    tmGerarToken: xNameSpaceURI := ConfigMsgDados.GerarToken.xmlns;
    tmEnviarEvento: xNameSpaceURI := ConfigMsgDados.EnviarEvento.xmlns;
    tmConsultarEvento: xNameSpaceURI := ConfigMsgDados.ConsultarEvento.xmlns;
    tmConsultarDFe: xNameSpaceURI := ConfigMsgDados.ConsultarDFe.xmlns;
    tmConsultarParam: xNameSpaceURI := ConfigMsgDados.ConsultarParam.xmlns;
    tmConsultarSeqRps: xNameSpaceURI := ConfigMsgDados.ConsultarSeqRps.xmlns;
    tmConsultarLinkNFSe: xNameSpaceURI := ConfigMsgDados.ConsultarLinkNFSe.xmlns;
  else
    xNameSpaceURI := FDefaultNameSpaceURI;
  end;
  TACBrNFSeX(FAOwner).SSL.NameSpaceURI := xNameSpaceURI
end;

procedure TACBrNFSeXProvider.SetNomeXSD(const aNome: string);
begin
  with ConfigSchemas do
  begin
    Recepcionar := aNome;
    ConsultarSituacao := aNome;
    ConsultarLote := aNome;
    ConsultarNFSeRps := aNome;
    ConsultarNFSe := aNome;
    ConsultarNFSePorChave := aNome;
    ConsultarNFSePorFaixa := aNome;
    ConsultarNFSeServicoPrestado := aNome;
    ConsultarNFSeServicoTomado := aNome;
    CancelarNFSe := aNome;
    GerarNFSe := aNome;
    RecepcionarSincrono := aNome;
    SubstituirNFSe := aNome;
    AbrirSessao := aNome;
    FecharSessao := aNome;
    Teste := aNome;
    GerarToken := aNome;
    EnviarEvento := aNome;
    ConsultarEvento := aNome;
    ConsultarDFe := aNome;
    ConsultarParam := aNome;
    ConsultarSeqRps := aNome;
    ConsultarLinkNFSe := aNome;

    Validar := True;
  end;
end;

procedure TACBrNFSeXProvider.SetXmlNameSpace(const aNameSpace: string);
begin
  with ConfigMsgDados do
  begin
    XmlRps.xmlns := aNameSpace;
    LoteRps.xmlns := aNameSpace;
    LoteRpsSincrono.xmlns := aNameSpace;
    ConsultarSituacao.xmlns := aNameSpace;
    ConsultarLote.xmlns := aNameSpace;
    ConsultarNFSeRps.xmlns := aNameSpace;
    ConsultarNFSe.xmlns := aNameSpace;
    ConsultarNFSePorChave.xmlns := aNameSpace;
    ConsultarNFSePorFaixa.xmlns := aNameSpace;
    ConsultarNFSeServicoPrestado.xmlns := aNameSpace;
    ConsultarNFSeServicoTomado.xmlns := aNameSpace;
    CancelarNFSe.xmlns := aNameSpace;
    GerarNFSe.xmlns := aNameSpace;
    SubstituirNFSe.xmlns := aNameSpace;
    AbrirSessao.xmlns := aNameSpace;
    FecharSessao.xmlns := aNameSpace;
    GerarToken.xmlns := aNameSpace;
    EnviarEvento.xmlns := aNameSpace;
    ConsultarEvento.xmlns := aNameSpace;
    ConsultarDFe.xmlns := aNameSpace;
    ConsultarParam.xmlns := aNameSpace;
    ConsultarSeqRps.xmlns := aNameSpace;
    ConsultarLinkNFSe.xmlns := aNameSpace;
  end;

  FDefaultNameSpaceURI := aNameSpace;
end;

function TACBrNFSeXProvider.SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4'],
                           [sLoteNaoRecibo, sLoteNaoProcessado,
                            sLoteProcessadoErro, sLoteProcessadoSucesso]);
end;

function TACBrNFSeXProvider.StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4'],
                           [sLoteNaoRecibo, sLoteNaoProcessado,
                            sLoteProcessadoErro, sLoteProcessadoSucesso]);
end;

function TACBrNFSeXProvider.SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['Lote Não Recebido', 'Lote Não Processado',
                            'Lote Processado com Erro', 'Lote Processado com Sucesso'],
                           [sLoteNaoRecibo, sLoteNaoProcessado,
                            sLoteProcessadoErro, sLoteProcessadoSucesso]);
end;

function TACBrNFSeXProvider.SimNaoToStr(const t: TnfseSimNao): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [snSim, snNao]);
end;

function TACBrNFSeXProvider.StrToSimNao(out ok: boolean;
  const s: string): TnfseSimNao;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', ''],
                           [snSim, snNao, snNao]);
end;

function TACBrNFSeXProvider.SituacaoTributariaToStr(
  const t: TnfseSituacaoTributaria): string;
begin
  Result := EnumeradoToStr(t,
                             ['1', '2', '3', ''],
                             [stRetencao, stNormal, stSubstituicao, stNenhum]);
end;

function TACBrNFSeXProvider.StrToSituacaoTributaria(out ok: boolean;
  const s: string): TnfseSituacaoTributaria;
begin
  Result := StrToEnumerado(ok, s,
                             ['1', '2', '3', ''],
                             [stRetencao, stNormal, stSubstituicao, stNenhum]);
end;

function TACBrNFSeXProvider.SituacaoTributariaDescricao(
  const t: TnfseSituacaoTributaria): string;
begin
  case t of
    stRetencao     : Result := '1 - Sim' ;
    stNormal       : Result := '2 - Nao' ;
    stSubstituicao : Result := '3 - Substituicao' ;
    stNenhum       : Result := '' ;
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.SimNaoDescricao(const t: TnfseSimNao): string;
begin
  if t = snSim then
    Result := 'Sim'
  else
    Result := 'Não';
end;

function TACBrNFSeXProvider.StrToSituacaoTrib(out ok: boolean;
  const s: string): TSituacaoTrib;
begin
  Result := StrToEnumerado(ok, s,
                           ['tp', 'tt', 'is', 'im', 'nt'],
                           [tsTributadaNoPrestador, tsTibutadaNoTomador, tsIsenta,
                            tsImune, tsNaoTributada]);
end;

function TACBrNFSeXProvider.SituacaoTribToStr(const t: TSituacaoTrib): string;
begin
  Result := EnumeradoToStr(t,
                           ['tp', 'tt', 'is', 'im', 'nt'],
                           [tsTributadaNoPrestador, tsTibutadaNoTomador, tsIsenta,
                            tsImune, tsNaoTributada]);
end;

function TACBrNFSeXProvider.GerarXml(const aNFSe: TNFSe; var aXml,
  aAlerts: string): Boolean;
var
  AWriter: TNFSeWClass;
begin
  AWriter := CriarGeradorXml(aNFSe);

  try
    with TACBrNFSeX(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        AWriter.Ambiente := taProducao
      else
        AWriter.Ambiente := taHomologacao;

      AWriter.CodMunEmit     := Configuracoes.Geral.CodigoMunicipio;
      AWriter.CNPJPrefeitura := Configuracoes.Geral.CNPJPrefeitura;

      AWriter.Usuario      := Configuracoes.Geral.Emitente.WSUser;
      AWriter.Senha        := Configuracoes.Geral.Emitente.WSSenha;
      AWriter.ChaveAcesso  := Configuracoes.Geral.Emitente.WSChaveAcesso;
      AWriter.ChaveAutoriz := Configuracoes.Geral.Emitente.WSChaveAutoriz;
      AWriter.FraseSecreta := Configuracoes.Geral.Emitente.WSFraseSecr;
      AWriter.Provedor     := Configuracoes.Geral.Provedor;
      AWriter.VersaoNFSe   := Configuracoes.Geral.Versao;
      AWriter.IniParams    := Configuracoes.Geral.PIniParams;

      pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

      AWriter.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
      AWriter.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
      AWriter.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
      AWriter.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    end;

    Result := AWriter.GerarXml;

    aXml := AWriter.ConteudoTxt;

    if aXml = '' then
      aXml := AWriter.Document.Xml;

    aAlerts := ACBrStr(AWriter.ListaDeAlertas.Text);
  finally
    AWriter.Destroy;
  end;
end;

function TACBrNFSeXProvider.LerXML(const aXML: String; var aNFSe: TNFSe;
  var ATipo: TtpXML; var aXmlTratado: string): Boolean;
var
  AReader: TNFSeRClass;
begin
  AReader := CriarLeitorXml(aNFSe);
  AReader.Arquivo := aXML;

  try
    with TACBrNFSeX(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        AReader.Ambiente := taProducao
      else
        AReader.Ambiente := taHomologacao;

      AReader.Provedor := Configuracoes.Geral.Provedor;
      AReader.IniParams := Configuracoes.Geral.PIniParams;
    end;

    Result := AReader.LerXml;
    ATipo := AReader.tpXML;
    aXmlTratado := AReader.Arquivo;
  finally
    AReader.Destroy;
  end;
end;

function TACBrNFSeXProvider.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  Result := EnumeradoToStr(t,
                         ['', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                          '10', '11', '12', '13', '14'],
                         [retNenhum, retMicroempresaMunicipal, retEstimativa,
                         retSociedadeProfissionais, retCooperativa,
                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                         retLucroReal, retLucroPresumido, retSimplesNacional,
                         retImune, retEmpresaIndividualRELI, retEmpresaPP,
                         retMicroEmpresario, retOutros]);
end;

function TACBrNFSeXProvider.StrToRegimeEspecialTributacao(out ok: boolean;
  const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                        ['', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                         '10', '11', '12', '13', '14'],
                        [retNenhum, retMicroempresaMunicipal, retEstimativa,
                         retSociedadeProfissionais, retCooperativa,
                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                         retLucroReal, retLucroPresumido, retSimplesNacional,
                         retImune, retEmpresaIndividualRELI, retEmpresaPP,
                         retMicroEmpresario, retOutros]);
end;

function TACBrNFSeXProvider.RegimeEspecialTributacaoDescricao(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  case t of
    retMicroempresaMunicipal     : Result := '1 - Microempresa municipal';
    retEstimativa                : Result := '2 - Estimativa';
    retSociedadeProfissionais    : Result := '3 - Sociedade de profissionais';
    retCooperativa               : Result := '4 - Cooperativa';
    retMicroempresarioIndividual : Result := '5 - Microempresário Individual (MEI)';
    retMicroempresarioEmpresaPP  : Result := '6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
    retLucroReal                 : Result := '7 - Lucro Real';
    retLucroPresumido            : Result := '8 - Lucro Presumido';
    retSimplesNacional           : Result := '9 - Simples Nacional';
    retImune                     : Result := '10 - Imune';
    retEmpresaIndividualRELI     : Result := '11 - Empresa Individual de Resp. Limitada (EIRELI)';
    retEmpresaPP                 : Result := '12 - Empresa de Pequeno Porte (EPP)';
    retMicroEmpresario           : Result := '13 - Microempresário';
    retOutros                    : Result := '14 - Outros/Sem Vinculo';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.ResponsavelRetencaoToStr(
  const t: TnfseResponsavelRetencao): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '', '2', ''],
                           [rtTomador, rtPrestador, rtIntermediario, rtNenhum]);
end;

function TACBrNFSeXProvider.StrToResponsavelRetencao(out ok: boolean;
  const s: string): TnfseResponsavelRetencao;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '', '2', ''],
                           [rtTomador, rtPrestador, rtIntermediario, rtNenhum]);
end;

function TACBrNFSeXProvider.ResponsavelRetencaoDescricao(
  const t: TnfseResponsavelRetencao): String;
begin
  case t of
    rtTomador       : Result := '1 - Tomador';
    rtIntermediario : Result := '2 - Intermediário';
    rtPrestador     : Result := '3 - Prestador';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.NaturezaOperacaoDescricao(
  const t: TnfseNaturezaOperacao): string;
begin
  case t of
    no1 : Result := '1 - Tributação no município';
    no2 : Result := '2 - Tributação fora do município';
    no3 : Result := '3 - Isenção';
    no4 : Result := '4 - Imune';
    no5 : Result := '5 - Exigibilidade susp. por decisão judicial';
    no6 : Result := '6 - Exigibilidade susp. por proced. adm.';

    no51 : Result := '5.1 - Tributacao No Municipio com retenção de ISS';
    no52 : Result := '9 - Tributacao No Municipio Sem Retenção de ISS';
    no58 : Result := '5.8 - Não tributável';
    no59 : Result := '7 - Simples Nacional (Dentro Estado)';
    no61 : Result := '6.1 - Tributacao No Municipio Com Retenção de ISS';
    no62 : Result := '6.2 - Tributacao No Municipio Sem Retenção de ISS';
    no63 : Result := '6.3 - Tributação fora do municipio com retenção de ISS';
    no64 : Result := '6.4 - Tributacao fora do municipio sem retenção de ISS';
    no68 : Result := '6.8 - Não tributável';
    no69 : Result := '8 - Simples Nacional (Fora Estado)';
    no78 : Result := '7.8 - Não tributável';
    no79 : Result := '7.9 - Imposto recolhido pelo regime único de arrecadação';

    no101 : Result := '101 - ISS devido no município';
    no103 : Result := '103 - ISENTO';
    no106 : Result := '106 - ISS FIXO';
    no107 : Result := '107 - ISS devido para o Municipio (Simples Nacional)';
    no108 : Result := '108 - ISS devido para outro Muinicipio (Simples Nacional)';
    no110 : Result := '110 - ISS retido pelo tomador devido para outros municipios (Simples Nacional)';
    no111 : Result := '111 - ISS RECOLHIDO NO PROJETO';
    no112 : Result := '112 - ISS NÃO TRIBUTÁVEL';
    no113 : Result := '113 - Nota Eletronica Avulsa';
    no114 : Result := '104 - ISS devido para origem prestado outro Município';
    no115 : Result := '115 - ISS devido para municipio, prestado em outro municipio';
    no121 : Result := '121 - ISS Fixo (Sociedade de Profissionais)';
    no201 : Result := '201 - ISS retido pelo tomador ou intermediário do serviço';
    no301 : Result := '301 - Operação imune, isenta ou não tributada';
    no501 : Result := '501 - ISS devido no município (Simples Nacional)';
    no511 : Result := '511 - Prestação de serviço no município - iss mensal sem retenção na fonte';
    no512 : Result := '512 - Prestação de serviço no município - iss mensal com retenção na fonte';
    no515 : Result := '515 - Prestação de serviço iss distribuido por rateio com retenção na fonte';
    no521 : Result := '521 - Construção civil - no município - iss mensal sem retenção na fonte';
    no522 : Result := '522 - Construção civil - no município - iss mensal com retenção na fonte';
    no539 : Result := '539 - Prestacao de serviço - recolhimento antecipado';
    no541 : Result := '541 - MEI (Simples Nacional)';
    no549 : Result := '549 - Prestacao de serviço - isento ou imune - nao tributavel';
    no601 : Result := '601 - ISS retido pelo tomador ou intermediário do serviço (Simples Nacional)';
    no611 : Result := '611 - Prestação de serviço em outro município - iss mensal sem retenção na fonte';
    no612 : Result := '612 - Prestação de serviço em outro município - iss mensal com retenção na fonte';
    no613 : Result := '613 - Prestação de serviço em outro município - iss mensal devido no local da prestaçâo';
    no615 : Result := '615 - Prestação de serviço em outro município - devido em outro município - semretenção na fonte';
    no621 : Result := '621 - Construção civil - outro município - iss mensal sem retenção na fonte';
    no622 : Result := '622 - Construção civil - em outro município - iss mensal com retenção na fonte';
    no701 : Result := '701 - Operação imune, isenta ou não tributada (Simples Nacional)';
    no711 : Result := '711 - Prestação de serviço para o exterior - iss mensal sem retenção na fonte';
    no712 : Result := '712 - Prestação de serviço para o exterior - iss mensal com retenção na fonte';
    no901 : Result := '901 - ISS retido ou sujeito à substituição tributária devido no município';
    no902 : Result := '902 - ISS retido ou sujeito à substituição tributária devido para outro município';
    no911 : Result := '911 - Prestação de serviço não enquadrada nas situações anteriores - sem retenção';
    no912 : Result := '912 - Prestação de serviço não enquadrada nas situações anteriores - com retenção';
    no921 : Result := '921 - ISS a ser recolhido pelo prestador do serviço';
    no931 : Result := '931 - Serviço imune, isento ou não tributado';
    no951 : Result := '951 - ISS retido ou sujeito à substituição tributária no município (prestador optante pelo Simples Nacional)';
    no952 : Result := '952 - ISS retido ou sujeito à substituição tributária, devido para outro município (prestador optante pelo Simples';
    no971 : Result := '971 - ISS a ser recolhido pelo prestador do serviço (prestador optante pelo Simples Nacional)';
    no981 : Result := '981 - Serviço imune, isento ou não tributado (prestador optante pelo Simples Nacional)';
    no991 : Result := '991 - Nota Fiscal de Serviços Avulsa (ISS pago antecipadamente pelo prestador)';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.TipoPessoaToStr(const t: TTipoPessoa): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [tpPFNaoIdentificada, tpPF, tpPJdoMunicipio,
                            tpPJforaMunicipio, tpPJforaPais]);
end;

function TACBrNFSeXProvider.StrToTipoPessoa(out ok: boolean;
  const s: string): TTipoPessoa;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [tpPFNaoIdentificada, tpPF, tpPJdoMunicipio,
                            tpPJforaMunicipio, tpPJforaPais]);
end;

function TACBrNFSeXProvider.ExigibilidadeISSDescricao(
  const t: TnfseExigibilidadeISS): string;
begin
  case t of
    exiExigivel                       : Result := '1 - Exigível';
    exiNaoIncidencia                  : Result := '2 - Não Incidência';
    exiIsencao                        : Result := '3 - Isenção';
    exiExportacao                     : Result := '4 - Exportação';
    exiImunidade                      : Result := '5 - Imunidade';
    exiSuspensaDecisaoJudicial        : Result := '6 - Suspensa Decisao Judicial';
    exiSuspensaProcessoAdministrativo : Result := '7 - Suspensa Processo Administrativo';
    exiISSFixo                        : Result := '8 - ISS Fixo';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.ExigibilidadeISSToStr(
  const t: TnfseExigibilidadeISS): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6', '7', '8'],
                           [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao,
                            exiImunidade, exiSuspensaDecisaoJudicial,
                            exiSuspensaProcessoAdministrativo, exiISSFixo]);
end;

function TACBrNFSeXProvider.StrToExigibilidadeISS(out ok: boolean;
  const s: string): TnfseExigibilidadeISS;
begin
  Result := StrToEnumerado(ok, s,
                          ['1', '2', '3', '4', '5', '6', '7', '8'],
                          [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao,
                           exiImunidade, exiSuspensaDecisaoJudicial,
                           exiSuspensaProcessoAdministrativo,exiISSFixo]);
end;

function TACBrNFSeXProvider.TipoRPSToStr(const t: TTipoRPS): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '0'],
                           [trRPS, trNFConjugada, trCupom, trNone]);
end;

function TACBrNFSeXProvider.StrToTipoRPS(out ok: boolean;
  const s: string): TTipoRPS;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '0'],
                           [trRPS, trNFConjugada, trCupom, trNone]);
end;

function TACBrNFSeXProvider.TributacaoDescricao(const t: TTributacao): String;
begin
  case t of
    ttIsentaISS           : Result := 'C - Isenta de ISS';
    ttNaoIncidencianoMunic: Result := 'E - Não Incidência no Município';
    ttImune               : Result := 'F - Imune';
    ttExigibilidadeSusp   : Result := 'K - Exigibilidade Susp.Dec.J/Proc.A';
    ttNaoTributavel       : Result := 'N - Não Tributável';
    ttTributavel          : Result := 'T - Tributável';
    ttTributavelFixo      : Result := 'G - Tributável Fixo';
    ttTributavelSN        : Result := 'H - Tributável S.N.';
    ttMEI                 : Result := 'M - Micro Empreendedor Individual(MEI)';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.TributacaoToStr(const t: TTributacao): string;
begin
  Result := EnumeradoToStr(t,
                           ['C', 'E', 'F', 'K', 'N', 'T', 'G', 'H', 'M'],
                           [ttIsentaISS, ttNaoIncidencianoMunic, ttImune,
                            ttExigibilidadeSusp, ttNaoTributavel, ttTributavel,
                            ttTributavelFixo, ttTributavelSN, ttMEI]);
end;

function TACBrNFSeXProvider.StrToTributacao(out ok: boolean;
  const s: string): TTributacao;
begin
  Result := StrToEnumerado(ok, s,
                           ['C', 'E', 'F', 'K', 'N', 'T', 'G', 'H', 'M'],
                           [ttIsentaISS, ttNaoIncidencianoMunic, ttImune,
                            ttExigibilidadeSusp, ttNaoTributavel, ttTributavel,
                            ttTributavelFixo, ttTributavelSN, ttMEI]);
end;

function TACBrNFSeXProvider.TipoDeducaoToStr(const t: TTipoDeducao): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6', '7'],
                 [tdNenhum, tdMateriais, tdPercentual, tdValor, tdPercMateriais,
                  tdVeiculacao, tdIntermediacao]);
end;

function TACBrNFSeXProvider.StrToTipoDeducao(out ok: Boolean;
  const s: string): TTipoDeducao;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6', '7'],
                 [tdNenhum, tdMateriais, tdPercentual, tdValor, tdPercMateriais,
                  tdVeiculacao, tdIntermediacao]);
end;

function TACBrNFSeXProvider.TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string;
begin
  Result := EnumeradoToStr(t,
                           ['T', 'F', 'A', 'B', 'M', 'N', 'X', 'V', 'P'],
                           [ttTribnoMun, ttTribforaMun,
                            ttTribnoMunIsento, ttTribforaMunIsento,
                            ttTribnoMunImune, ttTribforaMunImune,
                            ttTribnoMunSuspensa, ttTribforaMunSuspensa,
                            ttExpServicos]);
end;

function TACBrNFSeXProvider.StrToTipoTributacaoRPS(out ok: Boolean;
  const s: string): TTipoTributacaoRPS;
begin
  Result := StrToEnumerado(ok, s,
                           ['T', 'F', 'A', 'B', 'M', 'N', 'X', 'V', 'P'],
                           [ttTribnoMun, ttTribforaMun,
                            ttTribnoMunIsento, ttTribforaMunIsento,
                            ttTribnoMunImune, ttTribforaMunImune,
                            ttTribnoMunSuspensa, ttTribforaMunSuspensa,
                            ttExpServicos]);
end;

function TACBrNFSeXProvider.CondicaoPagToStr(
  const t: TnfseCondicaoPagamento): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoDebito,
                            cpCartaoCredito]);
end;

function TACBrNFSeXProvider.StrToCondicaoPag(out ok: boolean;
  const s: string): TnfseCondicaoPagamento;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoDebito,
                            cpCartaoCredito]);
end;

function TACBrNFSeXProvider.PrepararRpsParaLote(const aXml: string): string;
var
  i: Integer;
  Prefixo: string;
begin
  i := Pos('>', aXml) + 1;

  if ConfigMsgDados.PrefixoTS = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.PrefixoTS + ':';

  Result := '<' + Prefixo + 'Rps>' + Copy(aXml, i, Length(aXml));
end;

procedure TACBrNFSeXProvider.ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  AErro: TNFSeEventoCollectionItem;
  Erros, Schema: string;
begin
  if not ConfigSchemas.Validar Then Exit;

  SetNameSpaceURI(aMetodo);

  Erros := '';
  Schema := '';

  case aMetodo of
    tmRecepcionar: Schema := ConfigSchemas.Recepcionar;
    tmConsultarSituacao: Schema := ConfigSchemas.ConsultarSituacao;
    tmConsultarLote: Schema := ConfigSchemas.ConsultarLote;
    tmConsultarNFSePorRps: Schema := ConfigSchemas.ConsultarNFSeRps;

    tmConsultarNFSe: Schema := ConfigSchemas.ConsultarNFSe;
    tmConsultarNFSePorChave: Schema := ConfigSchemas.ConsultarNFSePorChave;
    tmConsultarNFSePorFaixa: Schema := ConfigSchemas.ConsultarNFSePorFaixa;
    tmConsultarNFSeServicoPrestado: Schema := ConfigSchemas.ConsultarNFSeServicoPrestado;
    tmConsultarNFSeServicoTomado: Schema := ConfigSchemas.ConsultarNFSeServicoTomado;

    tmCancelarNFSe: Schema := ConfigSchemas.CancelarNFSe;
    tmGerar: Schema := ConfigSchemas.GerarNFSe;
    tmRecepcionarSincrono: Schema := ConfigSchemas.RecepcionarSincrono;
    tmSubstituirNFSe: Schema := ConfigSchemas.SubstituirNFSe;
    tmAbrirSessao: Schema := ConfigSchemas.AbrirSessao;
    tmFecharSessao: Schema := ConfigSchemas.FecharSessao;
    tmGerarToken: Schema := ConfigSchemas.GerarToken;
    tmEnviarEvento: Schema := ConfigSchemas.EnviarEvento;
    tmConsultarEvento: Schema := ConfigSchemas.ConsultarEvento;
    tmConsultarDFe: Schema := ConfigSchemas.ConsultarDFe;
    tmConsultarParam: Schema := ConfigSchemas.ConsultarParam;
    tmConsultarSeqRps: Schema := ConfigSchemas.ConsultarSeqRps;
    tmConsultarLinkNFSe: Schema := ConfigSchemas.ConsultarLinkNFSe;
  else
    // tmTeste
    Schema := ConfigSchemas.Teste;
  end;

  if Schema = '***' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := ACBrStr(Desc001);
    Exit;
  end;

  if TACBrNFSeX(FAOwner).Configuracoes.Geral.MontarPathSchema then
    Schema := PathWithDelim(GetSchemaPath) + Schema
  else
    Schema := FAOwner.Configuracoes.Arquivos.PathSchemas + Schema;

  FAOwner.SSL.Validar(Response.ArquivoEnvio, Schema, Erros);

  if NaoEstaVazio(Erros) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod800;
    AErro.Descricao := ACBrStr(Desc800 + Erros);
  end;
end;

procedure TACBrNFSeXProvider.GeraLote;
begin
  GerarResponse.Erros.Clear;
  GerarResponse.Alertas.Clear;
  GerarResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  if GerarResponse.ModoEnvio = meAutomatico then
    GerarResponse.ModoEnvio := ConfigGeral.ModoEnvio;

  if GerarResponse.ModoEnvio <> meUnitario then
    GerarResponse.MaxRps := ConfigGeral.NumMaxRpsEnviar
  else
    GerarResponse.MaxRps := ConfigGeral.NumMaxRpsGerar;

  PrepararEmitir(GerarResponse);
  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(GerarResponse);
  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  case GerarResponse.ModoEnvio of
    meLoteAssincrono: ValidarSchema(EmiteResponse, tmRecepcionar);
    meLoteSincrono: ValidarSchema(EmiteResponse, tmRecepcionarSincrono);
    meTeste: ValidarSchema(EmiteResponse, tmTeste);
  else
    // meUnitario
    ValidarSchema(GerarResponse, tmGerar);
  end;

  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  GerarResponse.NomeArq := GerarResponse.NumeroLote + '-env-lot.xml';

  FAOwner.Gravar(GerarResponse.NomeArq, GerarResponse.ArquivoEnvio);

  GerarResponse.NomeArq := PathWithDelim(TACBrNFSeX(FAOwner).Configuracoes.Arquivos.PathSalvar) +
                           GerarResponse.NomeArq;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.Emite;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  EmiteResponse.Erros.Clear;
  EmiteResponse.Alertas.Clear;
  EmiteResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  if EmiteResponse.ModoEnvio = meAutomatico then
    EmiteResponse.ModoEnvio := ConfigGeral.ModoEnvio;

  EmiteResponse.MinRps := ConfigGeral.NumMinRpsEnviar;

  if EmiteResponse.ModoEnvio <> meUnitario then
    EmiteResponse.MaxRps := ConfigGeral.NumMaxRpsEnviar
  else
    EmiteResponse.MaxRps := ConfigGeral.NumMaxRpsGerar;

  PrepararEmitir(EmiteResponse);
  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(EmiteResponse);
  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  case EmiteResponse.ModoEnvio of
    meLoteAssincrono: ValidarSchema(EmiteResponse, tmRecepcionar);
    meLoteSincrono: ValidarSchema(EmiteResponse, tmRecepcionarSincrono);
    meTeste: ValidarSchema(EmiteResponse, tmTeste);
  else
    // meUnitario
    ValidarSchema(EmiteResponse, tmGerar);
  end;

  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      case EmiteResponse.ModoEnvio of
        meLoteAssincrono:
          begin
            AService := CriarServiceClient(tmRecepcionar);
            AService.Prefixo := EmiteResponse.NumeroLote;
            EmiteResponse.ArquivoRetorno := AService.Recepcionar(ConfigMsgDados.DadosCabecalho, EmiteResponse.ArquivoEnvio);
          end;

        meTeste:
          begin
            AService := CriarServiceClient(tmTeste);
            AService.Prefixo := EmiteResponse.NumeroLote;
            EmiteResponse.ArquivoRetorno := AService.TesteEnvio(ConfigMsgDados.DadosCabecalho, EmiteResponse.ArquivoEnvio);
          end;

        meLoteSincrono:
          begin
            AService := CriarServiceClient(tmRecepcionarSincrono);
            AService.Prefixo := EmiteResponse.NumeroLote;
            EmiteResponse.ArquivoRetorno := AService.RecepcionarSincrono(ConfigMsgDados.DadosCabecalho, EmiteResponse.ArquivoEnvio);
          end;
      else
        // meUnitario
        begin
          AService := CriarServiceClient(tmGerar);
          AService.Prefixo := EmiteResponse.NumeroLote;
          EmiteResponse.ArquivoRetorno := AService.GerarNFSe(ConfigMsgDados.DadosCabecalho, EmiteResponse.ArquivoEnvio);
          EmiteResponse.HtmlRetorno    := AService.HtmlRetorno;
        end;
      end;

      EmiteResponse.Sucesso := True;
      EmiteResponse.EnvelopeEnvio := AService.Envio;
      EmiteResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          EmiteResponse.EnvelopeEnvio := AService.Envio;
          EmiteResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := EmiteResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not EmiteResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoEmitir(EmiteResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaSituacao;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaSituacaoResponse.Erros.Clear;
  ConsultaSituacaoResponse.Alertas.Clear;
  ConsultaSituacaoResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultaSituacao);

  PrepararConsultaSituacao(ConsultaSituacaoResponse);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaSituacao(ConsultaSituacaoResponse);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaSituacaoResponse, tmConsultarSituacao);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarSituacao);
      AService.Prefixo := ConsultaSituacaoResponse.Protocolo;
      ConsultaSituacaoResponse.ArquivoRetorno := AService.ConsultarSituacao(ConfigMsgDados.DadosCabecalho,
                                                                        ConsultaSituacaoResponse.ArquivoEnvio);

      ConsultaSituacaoResponse.Sucesso := True;
      ConsultaSituacaoResponse.EnvelopeEnvio := AService.Envio;
      ConsultaSituacaoResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultaSituacaoResponse.EnvelopeEnvio := AService.Envio;
          ConsultaSituacaoResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultaSituacaoResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaSituacaoResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaSituacao(ConsultaSituacaoResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaLoteRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaLoteRpsResponse.Erros.Clear;
  ConsultaLoteRpsResponse.Alertas.Clear;
  ConsultaLoteRpsResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaLoteRps(ConsultaLoteRpsResponse);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaLoteRps(ConsultaLoteRpsResponse);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaLoteRpsResponse, tmConsultarLote);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarLote);
      AService.Prefixo := ConsultaLoteRpsResponse.Protocolo;
      ConsultaLoteRpsResponse.ArquivoRetorno := AService.ConsultarLote(ConfigMsgDados.DadosCabecalho, ConsultaLoteRpsResponse.ArquivoEnvio);

      ConsultaLoteRpsResponse.Sucesso := True;
      ConsultaLoteRpsResponse.EnvelopeEnvio := AService.Envio;
      ConsultaLoteRpsResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultaLoteRpsResponse.EnvelopeEnvio := AService.Envio;
          ConsultaLoteRpsResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultaLoteRpsResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaLoteRpsResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaLoteRps(ConsultaLoteRpsResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaNFSeporRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaNFSeporRpsResponse.Erros.Clear;
  ConsultaNFSeporRpsResponse.Alertas.Clear;
  ConsultaNFSeporRpsResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaNFSeporRpsResponse, tmConsultarNFSePorRps);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarNFSePorRps);
      AService.Prefixo := ConsultaNFSeporRpsResponse.NumeroRps + ConsultaNFSeporRpsResponse.SerieRps;
      ConsultaNFSeporRpsResponse.ArquivoRetorno := AService.ConsultarNFSePorRps(ConfigMsgDados.DadosCabecalho,
                                                                            ConsultaNFSeporRpsResponse.ArquivoEnvio);

      ConsultaNFSeporRpsResponse.Sucesso := True;
      ConsultaNFSeporRpsResponse.EnvelopeEnvio := AService.Envio;
      ConsultaNFSeporRpsResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultaNFSeporRpsResponse.EnvelopeEnvio := AService.Envio;
          ConsultaNFSeporRpsResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultaNFSeporRpsResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaNFSeporRpsResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultarEvento;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarEventoResponse.Erros.Clear;
  ConsultarEventoResponse.Alertas.Clear;
  ConsultarEventoResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultarEvento);

  PrepararConsultarEvento(ConsultarEventoResponse);
  if (ConsultarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultarEvento(ConsultarEventoResponse);
  if (ConsultarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultarEventoResponse, tmConsultarEvento);
  if (ConsultarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmConsultarEvento);
      AService.Prefixo := ConsultarEventoResponse.ChaveNFSe;

      ConsultarEventoResponse.ArquivoRetorno := AService.ConsultarEvento(ConfigMsgDados.DadosCabecalho,
                                                           ConsultarEventoResponse.ArquivoEnvio);

      ConsultarEventoResponse.Sucesso := True;
      ConsultarEventoResponse.EnvelopeEnvio := AService.Envio;
      ConsultarEventoResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultarEventoResponse.EnvelopeEnvio := AService.Envio;
          ConsultarEventoResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultarEventoResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultarEventoResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultarEvento(ConsultarEventoResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultarParam;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarParamResponse.Erros.Clear;
  ConsultarParamResponse.Alertas.Clear;
  ConsultarParamResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultarParam);

  PrepararConsultarParam(ConsultarParamResponse);
  if (ConsultarParamResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultarParam(ConsultarParamResponse);
  if (ConsultarParamResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultarParamResponse, tmConsultarParam);
  if (ConsultarParamResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmConsultarParam);
      AService.Prefixo := IntToStr(ConsultarParamResponse.CodigoMunicipio) +
        '-' + ParamMunicToStr(ConsultarParamResponse.tpParamMunic);

      ConsultarParamResponse.ArquivoRetorno := AService.ConsultarParam(ConfigMsgDados.DadosCabecalho,
                                                           ConsultarParamResponse.ArquivoEnvio);

      ConsultarParamResponse.Sucesso := True;
      ConsultarParamResponse.EnvelopeEnvio := AService.Envio;
      ConsultarParamResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultarParamResponse.EnvelopeEnvio := AService.Envio;
          ConsultarParamResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultarParamResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultarParamResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultarParam(ConsultarParamResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultarDFe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarDFeResponse.Erros.Clear;
  ConsultarDFeResponse.Alertas.Clear;
  ConsultarDFeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultarDFe);

  PrepararConsultarDFe(ConsultarDFeResponse);
  if (ConsultarDFeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultarDFe(ConsultarDFeResponse);
  if (ConsultarDFeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultarDFeResponse, tmConsultarDFe);
  if (ConsultarDFeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmConsultarDFe);
      AService.Prefixo := ConsultarDFeResponse.ChaveNFSe;

      ConsultarDFeResponse.ArquivoRetorno := AService.ConsultarDFe(ConfigMsgDados.DadosCabecalho,
                                                           ConsultarDFeResponse.ArquivoEnvio);

      ConsultarDFeResponse.Sucesso := True;
      ConsultarDFeResponse.EnvelopeEnvio := AService.Envio;
      ConsultarDFeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultarDFeResponse.EnvelopeEnvio := AService.Envio;
          ConsultarDFeResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultarDFeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultarDFeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultarDFe(ConsultarDFeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Prefixo: string;
begin
  ConsultaNFSeResponse.Erros.Clear;
  ConsultaNFSeResponse.Alertas.Clear;
  ConsultaNFSeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaNFSe(ConsultaNFSeResponse);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSe(ConsultaNFSeResponse);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaNFSeResponse, ConsultaNFSeResponse.Metodo);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(ConsultaNFSeResponse.Metodo);

      if (ConsultaNFSeResponse.InfConsultaNFSe.DataInicial > 0) and (ConsultaNFSeResponse.InfConsultaNFSe.DataFinal > 0) then
        Prefixo := FormatDateTime('yyyymmdd', ConsultaNFSeResponse.InfConsultaNFSe.DataInicial) +
                   FormatDateTime('yyyymmdd', ConsultaNFSeResponse.InfConsultaNFSe.DataFinal)
      else
        Prefixo := FormatFloat('000000000000000', StrToIntDef(ConsultaNFSeResponse.InfConsultaNFSe.NumeroIniNFSe, 0)) +
                   FormatFloat('000000000000000', StrToIntDef(ConsultaNFSeResponse.InfConsultaNFSe.NumeroFinNFSe, 0)) +
                   FormatFloat('000000', ConsultaNFSeResponse.InfConsultaNFSe.Pagina);

      AService.Prefixo := Prefixo;

      case ConsultaNFSeResponse.Metodo of
        tmConsultarNFSePorFaixa:
          begin
            ConsultaNFSeResponse.InfConsultaNFSe.tpConsulta := tcPorFaixa;
            ConsultaNFSeResponse.ArquivoRetorno := AService.ConsultarNFSePorFaixa(ConfigMsgDados.DadosCabecalho,
                                                                              ConsultaNFSeResponse.ArquivoEnvio);
          end;
        tmConsultarNFSeServicoPrestado:
          begin
            ConsultaNFSeResponse.InfConsultaNFSe.tpConsulta := tcServicoPrestado;
            ConsultaNFSeResponse.ArquivoRetorno := AService.ConsultarNFSeServicoPrestado(ConfigMsgDados.DadosCabecalho,
                                                                                     ConsultaNFSeResponse.ArquivoEnvio);
          end;
        tmConsultarNFSeServicoTomado:
          begin
            ConsultaNFSeResponse.InfConsultaNFSe.tpConsulta := tcServicoTomado;
            ConsultaNFSeResponse.ArquivoRetorno := AService.ConsultarNFSeServicoTomado(ConfigMsgDados.DadosCabecalho,
                                                                                   ConsultaNFSeResponse.ArquivoEnvio);
          end;
        tmConsultarNFSePorChave:
          begin
            ConsultaNFSeResponse.InfConsultaNFSe.tpConsulta := tcPorChave;
            ConsultaNFSeResponse.ArquivoRetorno := AService.ConsultarNFSePorChave(ConfigMsgDados.DadosCabecalho,
                                                                                  ConsultaNFSeResponse.ArquivoEnvio);
          end;
      else
        begin
          ConsultaNFSeResponse.InfConsultaNFSe.tpConsulta := tcPorNumero;
          ConsultaNFSeResponse.ArquivoRetorno := AService.ConsultarNFSe(ConfigMsgDados.DadosCabecalho,
                                                                    ConsultaNFSeResponse.ArquivoEnvio);
        end;
      end;

      ConsultaNFSeResponse.Sucesso := True;
      ConsultaNFSeResponse.EnvelopeEnvio := AService.Envio;
      ConsultaNFSeResponse.EnvelopeRetorno := AService.Retorno;
      ConsultaNFSeResponse.HtmlRetorno := AService.HtmlRetorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultaNFSeResponse.EnvelopeEnvio := AService.Envio;
          ConsultaNFSeResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultaNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSe(ConsultaNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaLinkNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaLinkNFSeResponse.Erros.Clear;
  ConsultaLinkNFSeResponse.Alertas.Clear;
  ConsultaLinkNFSeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaLinkNFSe(ConsultaLinkNFSeResponse);
  if (ConsultaLinkNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaLinkNFSe(ConsultaLinkNFSeResponse);
  if (ConsultaLinkNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaLinkNFSeResponse, ConsultaLinkNFSeResponse.Metodo);
  if (ConsultaLinkNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);
      AService := CriarServiceClient(ConsultaLinkNFSeResponse.Metodo);
      AService.Prefixo := ConsultaLinkNFSeResponse.Protocolo;

      ConsultaLinkNFSeResponse.ArquivoRetorno := AService.ConsultarLinkNFSe(ConfigMsgDados.DadosCabecalho, ConsultaLinkNFSeResponse.ArquivoEnvio);

      ConsultaLinkNFSeResponse.Sucesso := True;
      ConsultaLinkNFSeResponse.EnvelopeEnvio := AService.Envio;
      ConsultaLinkNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultaLinkNFSeResponse.EnvelopeEnvio := AService.Envio;
          ConsultaLinkNFSeResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultaLinkNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaLinkNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaLinkNFSe(ConsultaLinkNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.CancelaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  aConfig: TConfiguracoesNFSe;
begin
  CancelaNFSeResponse.Erros.Clear;
  CancelaNFSeResponse.Alertas.Clear;
  CancelaNFSeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeCancelamento);

  PrepararCancelaNFSe(CancelaNFSeResponse);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarCancelaNFSe(CancelaNFSeResponse);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(CancelaNFSeResponse, tmCancelarNFSe);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmCancelarNFSe);

      if CancelaNFSeResponse.InfCancelamento.NumeroNFSe <> '' then
        AService.Prefixo := CancelaNFSeResponse.InfCancelamento.NumeroNFSe
      else
        AService.Prefixo := CancelaNFSeResponse.InfCancelamento.ChaveNFSe;

      aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

      AService.Path := aConfig.Arquivos.GetPathCan(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

      CancelaNFSeResponse.ArquivoRetorno := AService.Cancelar(ConfigMsgDados.DadosCabecalho, CancelaNFSeResponse.ArquivoEnvio);

      CancelaNFSeResponse.Sucesso := True;
      CancelaNFSeResponse.EnvelopeEnvio := AService.Envio;
      CancelaNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          CancelaNFSeResponse.EnvelopeEnvio := AService.Envio;
          CancelaNFSeResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := CancelaNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not CancelaNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoCancelaNFSe(CancelaNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.SubstituiNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Cancelamento: TNFSeCancelaNFSeResponse;
begin
  SubstituiNFSeResponse.Erros.Clear;
  SubstituiNFSeResponse.Alertas.Clear;
  SubstituiNFSeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeSubstituicao);

  Cancelamento := TNFSeCancelaNFSeResponse.Create;

  try
    Cancelamento.Erros.Clear;
    Cancelamento.Alertas.Clear;
    Cancelamento.Resumos.Clear;

    with Cancelamento.InfCancelamento do
    begin
      NumeroNFSe := SubstituiNFSeResponse.InfCancelamento.NumeroNFSe;
      SerieNFSe := SubstituiNFSeResponse.InfCancelamento.SerieNFSe;
      CodCancelamento := SubstituiNFSeResponse.InfCancelamento.CodCancelamento;
      MotCancelamento := SubstituiNFSeResponse.InfCancelamento.MotCancelamento;
      NumeroLote := SubstituiNFSeResponse.InfCancelamento.NumeroLote;
      CodVerificacao := SubstituiNFSeResponse.InfCancelamento.CodVerificacao;
    end;

    PrepararCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AssinarCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    SubstituiNFSeResponse.PedCanc := Cancelamento.ArquivoEnvio;
    SubstituiNFSeResponse.PedCanc := SepararDados(SubstituiNFSeResponse.PedCanc, 'CancelarNfseEnvio', False);
  finally
    FreeAndNil(Cancelamento);
  end;

  PrepararSubstituiNFSe(SubstituiNFSeResponse);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarSubstituiNFSe(SubstituiNFSeResponse);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(SubstituiNFSeResponse, tmSubstituirNFSe);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmSubstituirNFSe);
      AService.Prefixo := SubstituiNFSeResponse.InfCancelamento.NumeroNFSe;
      SubstituiNFSeResponse.ArquivoRetorno := AService.SubstituirNFSe(ConfigMsgDados.DadosCabecalho,
                                                                  SubstituiNFSeResponse.ArquivoEnvio);

      SubstituiNFSeResponse.Sucesso := True;
      SubstituiNFSeResponse.EnvelopeEnvio := AService.Envio;
      SubstituiNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          SubstituiNFSeResponse.EnvelopeEnvio := AService.Envio;
          SubstituiNFSeResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := SubstituiNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not SubstituiNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoSubstituiNFSe(SubstituiNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.GerarToken;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  GerarTokenResponse.Erros.Clear;
  GerarTokenResponse.Alertas.Clear;
  GerarTokenResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeGerarToken);

  PrepararGerarToken(GerarTokenResponse);
  if (GerarTokenResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarGerarToken(GerarTokenResponse);
  if (GerarTokenResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(GerarTokenResponse, tmGerarToken);
  if (GerarTokenResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmGerarToken);

      GerarTokenResponse.ArquivoRetorno := AService.GerarToken(ConfigMsgDados.DadosCabecalho,
                                                           GerarTokenResponse.ArquivoEnvio);

      GerarTokenResponse.Sucesso := True;
      GerarTokenResponse.EnvelopeEnvio := AService.Envio;
      GerarTokenResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          GerarTokenResponse.EnvelopeEnvio := AService.Envio;
          GerarTokenResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := GerarTokenResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not GerarTokenResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoGerarToken(GerarTokenResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.EnviarEvento;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  aConfig: TConfiguracoesNFSe;
begin
  EnviarEventoResponse.Erros.Clear;
  EnviarEventoResponse.Alertas.Clear;
  EnviarEventoResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeGerarToken);

  PrepararEnviarEvento(EnviarEventoResponse);
  if (EnviarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEnviarEvento(EnviarEventoResponse);
  if (EnviarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(EnviarEventoResponse, tmEnviarEvento);
  if (EnviarEventoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmEnviarEvento);
      AService.Prefixo := EnviarEventoResponse.InfEvento.pedRegEvento.ID;

      aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

      AService.Path := aConfig.Arquivos.GetPathEvento(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

      EnviarEventoResponse.ArquivoRetorno := AService.EnviarEvento(ConfigMsgDados.DadosCabecalho,
                                                           EnviarEventoResponse.ArquivoEnvio);

      EnviarEventoResponse.Sucesso := True;
      EnviarEventoResponse.EnvelopeEnvio := AService.Envio;
      EnviarEventoResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          EnviarEventoResponse.EnvelopeEnvio := AService.Envio;
          EnviarEventoResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := EnviarEventoResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not EnviarEventoResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoEnviarEvento(EnviarEventoResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultarSeqRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarSeqRpsResponse.Erros.Clear;
  ConsultarSeqRpsResponse.Alertas.Clear;
  ConsultarSeqRpsResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultarSeqRps);

  PrepararConsultarSeqRps(ConsultarSeqRpsResponse);
  if (ConsultarSeqRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultarSeqRps(ConsultarSeqRpsResponse);
  if (ConsultarSeqRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultarSeqRpsResponse, tmConsultarSeqRps);
  if (ConsultarSeqRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(tmConsultarSeqRps);

      ConsultarSeqRpsResponse.ArquivoRetorno := AService.ConsultarSeqRps(ConfigMsgDados.DadosCabecalho,
                                                           ConsultarSeqRpsResponse.ArquivoEnvio);

      ConsultarSeqRpsResponse.Sucesso := True;
      ConsultarSeqRpsResponse.EnvelopeEnvio := AService.Envio;
      ConsultarSeqRpsResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultarSeqRpsResponse.EnvelopeEnvio := AService.Envio;
          ConsultarSeqRpsResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultarSeqRpsResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultarSeqRpsResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultarSeqRps(ConsultarSeqRpsResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.AssinarConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarLinkNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarLinkNFSe.DocElemento,
      ConfigMsgDados.ConsultarLinkNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.CancelarNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.CancelarNFSe.DocElemento,
      ConfigMsgDados.CancelarNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarLote then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarLote.DocElemento,
      ConfigMsgDados.ConsultarLote.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa:
      if not ConfigAssinar.ConsultarNFSePorFaixa then Exit;

    tcServicoPrestado:
      if not ConfigAssinar.ConsultarNFSeServicoPrestado then Exit;

    tcServicoTomado:
      if not ConfigAssinar.ConsultarNFSeServicoTomado then Exit;
  else
    if not ConfigAssinar.ConsultarNFSe then Exit;
  end;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    case Response.InfConsultaNFSe.tpConsulta of
      tcPorPeriodo,
      tcPorFaixa:
        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento,
          ConfigMsgDados.ConsultarNFSePorFaixa.InfElemento, '', '', '', IdAttr);

      tcServicoPrestado:
        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento,
          ConfigMsgDados.ConsultarNFSeServicoPrestado.InfElemento, '', '', '', IdAttr);

      tcServicoTomado:
        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento,
          ConfigMsgDados.ConsultarNFSeServicoTomado.InfElemento, '', '', '', IdAttr);
    else
      Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
        Prefixo + ConfigMsgDados.ConsultarNFSe.DocElemento,
        ConfigMsgDados.ConsultarNFSe.InfElemento, '', '', '', IdAttr);
    end;
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSeRps then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSeRps.DocElemento,
      ConfigMsgDados.ConsultarNFSeRps.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarEvento then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarEvento.DocElemento,
      ConfigMsgDados.ConsultarEvento.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultarParam(
  Response: TNFSeConsultarParamResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarParam then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarParam.DocElemento,
      ConfigMsgDados.ConsultarParam.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarSituacao then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarSituacao.DocElemento,
      ConfigMsgDados.ConsultarSituacao.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarEmitir(Response: TNFSeEmiteResponse);
var
  IdAttr, Prefixo, PrefixoTS: string;
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.ModoEnvio of
    meTeste,
    meLoteSincrono,
    meLoteAssincrono:
      if not ConfigAssinar.LoteRps then Exit;
  else
    if not ConfigAssinar.LoteGerarNFSe then Exit;
  end;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  if ConfigMsgDados.PrefixoTS = '' then
    PrefixoTS := ''
  else
    PrefixoTS := ConfigMsgDados.PrefixoTS + ':';

  try
    case Response.ModoEnvio of
      meLoteSincrono:
        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.LoteRpsSincrono.DocElemento,
          Prefixo + ConfigMsgDados.LoteRpsSincrono.InfElemento, '', '', '', IdAttr);

      meTeste,
      meLoteAssincrono:
        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.LoteRps.DocElemento,
          {Prefixo + }ConfigMsgDados.LoteRps.InfElemento, '', '', '', IdAttr);
    else
      Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
        Prefixo + ConfigMsgDados.GerarNFSe.DocElemento,
        PrefixoTS + ConfigMsgDados.GerarNFSe.InfElemento, '', '', '', IdAttr);
    end;
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.SubstituirNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.SubstituirNFSe.DocElemento,
      ConfigMsgDados.SubstituirNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.GerarToken then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.GerarToken.DocElemento,
      ConfigMsgDados.GerarToken.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.EnviarEvento then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.EnviarEvento.DocElemento,
      ConfigMsgDados.EnviarEvento.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarDFe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarDFe.DocElemento,
      ConfigMsgDados.ConsultarDFe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarParam then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarParam.DocElemento,
      ConfigMsgDados.ConsultarParam.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

end.
