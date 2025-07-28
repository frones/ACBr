{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeXProviderBase;

interface

uses
  SysUtils, Classes,
  ACBrBase, ACBrDFe,
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
    FPrefixoTS: string;

    procedure Configuracao; virtual;
    procedure CarregarURL; virtual;
    procedure SetNomeXSD(const aNome: string);
    procedure SetXmlNameSpace(const aNameSpace: string);
    procedure SetNameSpaceURI(const aMetodo: TMetodo);
    procedure SalvarXmlRps(aNota: TNotaFiscal);
    procedure SalvarXmlNfse(aNota: TNotaFiscal); overload;
    procedure SalvarXmlNfse(const NumeroNFSe: string; const aXml: AnsiString); overload;
    procedure SalvarPDFNfse(const aNome: string; const aPDF: AnsiString);
    procedure SalvarXmlEvento(const aNome: string; const aEvento: AnsiString; PathNome: string);
    procedure SalvarXmlCancelamento(const aNome, aCancelamento: string; PathNome: string);

    function CarregarXmlNfse(aNota: TNotaFiscal; const aXml: string): TNotaFiscal;

    function GetWebServiceURL(const AMetodo: TMetodo): string;
    function SetIdSignatureValue(const ConteudoXml, docElement, IdAttr: string): string;  virtual;

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

    //metodos para gera��o e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); virtual; abstract;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarEmitir(Response: TNFSeEmiteResponse); virtual;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultaSituacao
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaSituacao(Response: TNFSeConsultaSituacaoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaNFSe(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultaLinkNFSe
    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo SubstituirNFSe
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo GerarToken
    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); virtual; abstract;
    procedure GerarMsgDadosGerarToken(Response: TNFSeGerarTokenResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarGerarToken(Response: TNFSeGerarTokenResponse); virtual;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo EnviarEvento
    procedure PrepararEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual; abstract;
    procedure GerarMsgDadosEnviarEvento(Response: TNFSeEnviarEventoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual;
    procedure TratarRetornoEnviarEvento(Response: TNFSeEnviarEventoResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultarEvento
    procedure PrepararConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarEvento(Response: TNFSeConsultarEventoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual;
    procedure TratarRetornoConsultarEvento(Response: TNFSeConsultarEventoResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultarDFe
    procedure PrepararConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarDFe(Response: TNFSeConsultarDFeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual;
    procedure TratarRetornoConsultarDFe(Response: TNFSeConsultarDFeResponse); virtual; abstract;

    //metodos para gera��o e tratamento dos dados do metodo ConsultarParam
    procedure PrepararConsultarParam(Response: TNFSeConsultarParamResponse); virtual; abstract;
    procedure GerarMsgDadosConsultarParam(Response: TNFSeConsultarParamResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultarParam(Response: TNFSeConsultarParamResponse); virtual;
    procedure TratarRetornoConsultarParam(Response: TNFSeConsultarParamResponse); virtual; abstract;

    //m�todo usado para consultar o �ltimo rps convertido em nfse
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

    function GerarIni(const aNFSe: TNFSe): string; virtual;
    function LerIni(const aINI: String; var aNFSe: TNFSe): Boolean; virtual;

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

    function SimNaoOpcToStr(const t: TnfseSimNaoOpc): string; virtual;
    function StrToSimNaoOpc(out ok: boolean; const s: string): TnfseSimNaoOpc; virtual;

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

    function DeducaoPorToStr(const t: TDeducaoPor): string; virtual;
    function StrToDeducaoPor(out ok: Boolean; const s: string): TDeducaoPor; virtual;

    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string; virtual;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS; virtual;

    function CondicaoPagToStr(const t: TnfseCondicaoPagamento): string; virtual;
    function StrToCondicaoPag(out ok: boolean; const s: string): TnfseCondicaoPagamento; virtual;

    function StatusRPSToStr(const t: TStatusRPS): string; virtual;
    function StrToStatusRPS(out ok: boolean; const s: string): TStatusRPS; virtual;
  end;

implementation

uses
  IniFiles,
  ACBrConsts,
  ACBrUtil.DateTime,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrXmlBase, ACBrDFeException, ACBrDFeUtil,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts;

{ TACBrNFSeXProvider }

constructor TACBrNFSeXProvider.Create(AOwner: TACBrDFe);
begin
  inherited Create;

  FAOwner := AOwner;
  if not Assigned(FAOwner) then
    raise EACBrDFeException.Create('Componente ACBrNFSeX n�o informado');

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
        // M�todos padr�es da vers�o 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // M�todos padr�es da vers�o 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // M�todos que por padr�o n�o existem na vers�o 1 e 2 do layout da ABRASF
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
        // M�todos padr�es da vers�o 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // M�todos padr�es da vers�o 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // M�todos que por padr�o n�o existem na vers�o 1 e 2 do layout da ABRASF
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

function TACBrNFSeXProvider.SetIdSignatureValue(const ConteudoXml, docElement,
  IdAttr: string): string;
begin
  if ConfigAssinar.IdSignatureValue <> '' then
    Result := ' Id="' + ConfigAssinar.IdSignatureValue +
              EncontrarURI(ConteudoXml, docElement, IdAttr) + '"'
  else
    Result := '';
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
  // Inicializa os par�metros de configura��o: Geral
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
    ImprimirOptanteSN := True;

    Autenticacao.RequerCertificado := True;
    Autenticacao.RequerLogin := False;
    Autenticacao.RequerChaveAcesso := False;
    Autenticacao.RequerChaveAutorizacao := False;
    Autenticacao.RequerFraseSecreta := False;

    ServicosDisponibilizados.EnviarLoteAssincrono := False;
    ServicosDisponibilizados.EnviarLoteSincrono := False;
    ServicosDisponibilizados.EnviarUnitario := False;
    ServicosDisponibilizados.ConsultarSituacao := False;
    ServicosDisponibilizados.ConsultarLote := False;
    ServicosDisponibilizados.ConsultarRps := False;
    ServicosDisponibilizados.ConsultarNfse := False;
    ServicosDisponibilizados.ConsultarFaixaNfse := False;
    ServicosDisponibilizados.ConsultarServicoPrestado := False;
    ServicosDisponibilizados.ConsultarServicoTomado := False;
    ServicosDisponibilizados.CancelarNfse := False;
    ServicosDisponibilizados.SubstituirNfse := False;
    ServicosDisponibilizados.GerarToken := False;
    ServicosDisponibilizados.EnviarEvento := False;
    ServicosDisponibilizados.ConsultarEvento := False;
    ServicosDisponibilizados.ConsultarDFe := False;
    ServicosDisponibilizados.ConsultarParam := False;
    ServicosDisponibilizados.ConsultarSeqRps := False;
    ServicosDisponibilizados.ConsultarLinkNfse := False;
    ServicosDisponibilizados.ConsultarNfseChave := False;
    ServicosDisponibilizados.TestarEnvio := False;

    Particularidades.PermiteMaisDeUmServico := False;
    Particularidades.PermiteTagOutrasInformacoes := False;
    Particularidades.AtendeReformaTributaria := False;

    Provedor := TACBrNFSeX(FAOwner).Configuracoes.Geral.Provedor;
    Versao := TACBrNFSeX(FAOwner).Configuracoes.Geral.Versao;
    xMunicipio := TACBrNFSeX(FAOwner).Configuracoes.Geral.xMunicipio;

    if TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
      Ambiente := taProducao
    else
      Ambiente := taHomologacao;

    CodIBGE := IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio);
    IniTabServicos := TACBrNFSeX(FAOwner).Configuracoes.Arquivos.IniTabServicos;
  end;

  // Inicializa os par�metros de configura��o: MsgDados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';
    PrefixoTS := '';

    UsarNumLoteConsLote := False;

    // Usado para gera��o do Xml do Rps
    XmlRps.xmlns := '';
    XmlRps.InfElemento := 'InfRps';
    XmlRps.DocElemento := 'Rps';

    // Usado para gera��o do Envio do Lote em modo ass�ncrono
    LoteRps.xmlns := '';
    LoteRps.InfElemento := 'LoteRps';
    LoteRps.DocElemento := 'EnviarLoteRpsEnvio';

    // Usado para gera��o do Envio do Lote em modo Sincrono
    LoteRpsSincrono.xmlns := '';
    LoteRpsSincrono.InfElemento := 'LoteRps';
    LoteRpsSincrono.DocElemento := 'EnviarLoteRpsSincronoEnvio';

    // Usado para gera��o da Consulta a Situa��o do Lote
    ConsultarSituacao.xmlns := '';
    ConsultarSituacao.InfElemento := '';
    ConsultarSituacao.DocElemento := 'ConsultarSituacaoLoteRpsEnvio';

    // Usado para gera��o da Consulta do Lote
    ConsultarLote.xmlns := '';
    ConsultarLote.InfElemento := '';
    ConsultarLote.DocElemento := 'ConsultarLoteRpsEnvio';

    // Usado para gera��o da Consulta da NFSe por RPS
    ConsultarNFSeRps.xmlns := '';
    ConsultarNFSeRps.InfElemento := '';
    ConsultarNFSeRps.DocElemento := 'ConsultarNfseRpsEnvio';

    // Usado para gera��o da Consulta da NFSe
    ConsultarNFSe.xmlns := '';
    ConsultarNFSe.InfElemento := '';
    ConsultarNFSe.DocElemento := 'ConsultarNfseEnvio';

    // Usado para gera��o da Consulta da NFSe Por Chave
    ConsultarNFSePorChave.xmlns := '';
    ConsultarNFSePorChave.InfElemento := '';
    ConsultarNFSePorChave.DocElemento := '';

    // Usado para gera��o da Consulta da NFSe Por Faixa
    ConsultarNFSePorFaixa.xmlns := '';
    ConsultarNFSePorFaixa.InfElemento := '';
    ConsultarNFSePorFaixa.DocElemento := 'ConsultarNfseFaixaEnvio';

    // Usado para gera��o da Consulta da NFSe Servico Prestado
    ConsultarNFSeServicoPrestado.xmlns := '';
    ConsultarNFSeServicoPrestado.InfElemento := '';
    ConsultarNFSeServicoPrestado.DocElemento := 'ConsultarNfseServicoPrestadoEnvio';

    // Usado para gera��o da Consulta da NFSe Servico Tomado
    ConsultarNFSeServicoTomado.xmlns := '';
    ConsultarNFSeServicoTomado.InfElemento := '';
    ConsultarNFSeServicoTomado.DocElemento := 'ConsultarNfseServicoTomadoEnvio';

    // Usado para gera��o do Cancelamento
    CancelarNFSe.xmlns := '';
    CancelarNFSe.InfElemento := 'InfPedidoCancelamento';
    CancelarNFSe.DocElemento := 'Pedido';

    // Usado para gera��o do Gerar
    GerarNFSe.xmlns := '';
    GerarNFSe.InfElemento := '';
    GerarNFSe.DocElemento := 'GerarNfseEnvio';

    // Usado para gera��o do Substituir
    SubstituirNFSe.xmlns := '';
    SubstituirNFSe.InfElemento := 'SubstituicaoNfse';
    SubstituirNFSe.DocElemento := 'SubstituirNfseEnvio';

    // Usado para gera��o da Abertura de Sess�o
    AbrirSessao.xmlns := '';
    AbrirSessao.InfElemento := '';
    AbrirSessao.DocElemento := '';

    // Usado para gera��o do Fechamento de Sess�o
    FecharSessao.xmlns := '';
    FecharSessao.InfElemento := '';
    FecharSessao.DocElemento := '';

    // Usado para gera��o do Gerar Token
    GerarToken.xmlns := '';
    GerarToken.InfElemento := '';
    GerarToken.DocElemento := '';

    // Usado para gera��o do Enviar Evento
    EnviarEvento.xmlns := '';
    EnviarEvento.InfElemento := '';
    EnviarEvento.DocElemento := '';

    // Usado para gera��o do Consultar Evento
    ConsultarEvento.xmlns := '';
    ConsultarEvento.InfElemento := '';
    ConsultarEvento.DocElemento := '';

    // Usado para gera��o do Consultar Evento
    ConsultarDFe.xmlns := '';
    ConsultarDFe.InfElemento := '';
    ConsultarDFe.DocElemento := '';

    // Usado para gera��o do Consultar Par�metros
    ConsultarParam.xmlns := '';
    ConsultarParam.InfElemento := '';
    ConsultarParam.DocElemento := '';

    // Usado para gera��o do Consultar Sequencia de Rps
    ConsultarSeqRps.xmlns := '';
    ConsultarSeqRps.InfElemento := '';
    ConsultarSeqRps.DocElemento := '';

    // Usado para gera��o do Consultar Link da NFS-e
    ConsultarLinkNFSe.xmlns := '';
    ConsultarLinkNFSe.InfElemento := '';
    ConsultarLinkNFSe.DocElemento := '';
  end;

  // Inicializa os par�metros de configura��o: Assinar
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
    IdSignatureValue := '';
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

function TACBrNFSeXProvider.CarregarXmlNfse(aNota: TNotaFiscal; const aXml: string): TNotaFiscal;
begin
  if Assigned(ANota) then
    ANota.XmlNfse := aXml
  else
  begin
    TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(aXml, False);
    ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];

    if (aNota.XmlRps <> '') and (ANota.XmlNfse = '') then
      ANota.XmlNfse := aNota.XmlRps;
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
    begin
      aNota.NomeArqRps := StringReplace(aNota.NomeArqRps, '.xml', Extensao, [rfReplaceAll]);

      WriteToTXT(aNota.NomeArqRps, aNota.XmlRps, False, False);
    end
    else
      TACBrNFSeX(FAOwner).Gravar(aNota.NomeArqRps, aNota.XmlRps, '', ConteudoEhXml);
  end;
end;

procedure TACBrNFSeXProvider.SalvarXmlNfse(aNota: TNotaFiscal);
var
  aPath, aNomeArq, Extensao, aXML: string;
  aConfig: TConfiguracoesNFSe;
  ConteudoEhXml: Boolean;
  Data: TDateTime;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  if aConfig.Arquivos.EmissaoPathNFSe then
    Data := aNota.NFSe.DataEmissao
  else
    Data := Now;

  aPath := aConfig.Arquivos.GetPathNFSe(Data, aConfig.Geral.Emitente.CNPJ,
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

    aXml := aNota.XmlNfse;

    if aXml = '' then
      aXml := aNota.XmlEspelho;

    if aXml = '' then
      aXml := aNota.XmlRps;

    if ConfigGeral.FormatoArqNota <> tfaXml then
    begin
      aXml := RemoverDeclaracaoXML(aXml);
      ConteudoEhXml := False;
    end
    else
      ConteudoEhXml := True;

    if not ConteudoEhXml then
    begin
      aNota.NomeArq := StringReplace(aNota.NomeArq, '.xml', Extensao, [rfReplaceAll]);

      WriteToTXT(aNota.NomeArq, aXml, False, False);
    end
    else
      TACBrNFSeX(FAOwner).Gravar(aNota.NomeArq, aXml, '', ConteudoEhXml);
  end;
end;

procedure TACBrNFSeXProvider.SalvarXmlNfse(const NumeroNFSe: string;
  const aXml: AnsiString);
var
  aPath, aNomeArq, Extensao, AuxXml: string;
  aConfig: TConfiguracoesNFSe;
  ConteudoEhXml: Boolean;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathNFSe(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

  aNomeArq := NumeroNFSe + '-nfse.xml';
  aNomeArq := PathWithDelim(aPath) + aNomeArq;

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
      AuxXml := RemoverDeclaracaoXML(aXml);
      ConteudoEhXml := False;
    end
    else
    begin
      AuxXml := RemoverDeclaracaoXML(aXml);
      ConteudoEhXml := True;
    end;

    if not ConteudoEhXml then
    begin
      aNomeArq := StringReplace(aNomeArq, '.xml', Extensao, [rfReplaceAll]);

      WriteToTXT(aNomeArq, AuxXml, False, False);
    end
    else
      TACBrNFSeX(FAOwner).Gravar(aNomeArq, AuxXml, '', ConteudoEhXml);
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
  const aEvento: AnsiString; PathNome: string);
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
  PathNome := aNomeArq;

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

procedure TACBrNFSeXProvider.SalvarXmlCancelamento(const aNome,
  aCancelamento: string; PathNome: string);
var
  aPath, aNomeArq, Extensao: string;
  aConfig: TConfiguracoesNFSe;
  ConteudoEhXml: Boolean;
  ArqCancelamento: string;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathCan(0, aConfig.Geral.Emitente.CNPJ,
                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

  aNomeArq := PathWithDelim(aPath) + aNome + '.xml';
  PathNome := aNomeArq;

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
      ArqCancelamento := RemoverDeclaracaoXML(aCancelamento);
      ConteudoEhXml := False;
    end
    else
    begin
      ArqCancelamento := aCancelamento;
      ConteudoEhXml := True;
    end;

    if not ConteudoEhXml then
      aNomeArq := StringReplace(aNomeArq, '.xml', Extensao, [rfReplaceAll]);

    WriteToTXT(aNomeArq, ArqCancelamento, False, False);
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
                           ['Lote N�o Recebido', 'Lote N�o Processado',
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

function TACBrNFSeXProvider.SimNaoOpcToStr(const t: TnfseSimNaoOpc): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', ''],
                           [snoSim, snoNao, snoNenhum]);
end;

function TACBrNFSeXProvider.StrToSimNaoOpc(out ok: boolean;
  const s: string): TnfseSimNaoOpc;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', ''],
                           [snoSim, snoNao, snoNenhum]);
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
    Result := 'N�o';
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

      AWriter.CodMunEmit := Configuracoes.Geral.CodigoMunicipio;
      AWriter.CNPJPrefeitura := Configuracoes.Geral.CNPJPrefeitura;

      AWriter.Usuario := Configuracoes.Geral.Emitente.WSUser;
      AWriter.Senha := Configuracoes.Geral.Emitente.WSSenha;
      AWriter.ChaveAcesso := Configuracoes.Geral.Emitente.WSChaveAcesso;
      AWriter.ChaveAutoriz := Configuracoes.Geral.Emitente.WSChaveAutoriz;
      AWriter.FraseSecreta := Configuracoes.Geral.Emitente.WSFraseSecr;
      AWriter.Provedor := Configuracoes.Geral.Provedor;
      AWriter.VersaoNFSe := Configuracoes.Geral.Versao;
      AWriter.IniParams := Configuracoes.Geral.PIniParams;
      AWriter.FormatoDiscriminacao := Configuracoes.Geral.FormatoDiscriminacao;

      TimeZoneConf.Assign(Configuracoes.WebServices.TimeZoneConf);

      AWriter.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
      AWriter.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
      AWriter.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
      AWriter.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
      AWriter.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;
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

    if aNFSe.Tomador.RazaoSocial = '' then
      aNFSe.Tomador.RazaoSocial := 'Tomador N�o Identificado';
  finally
    AReader.Destroy;
  end;
end;

function TACBrNFSeXProvider.GerarIni(const aNFSe: TNFSe): string;
var
  AWriter: TNFSeWClass;
begin
  AWriter := CriarGeradorXml(aNFSe);

  try
    Result := AWriter.GerarIni;

  finally
    AWriter.Destroy;
  end;
end;

function TACBrNFSeXProvider.LerIni(const aINI: String;
  var aNFSe: TNFSe): Boolean;
var
  AReader: TNFSeRClass;
begin
  AReader := CriarLeitorXml(aNFSe);
  AReader.Arquivo := aINI;

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

    Result := AReader.LerIni;
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
    retMicroempresarioIndividual : Result := '5 - Microempres�rio Individual (MEI)';
    retMicroempresarioEmpresaPP  : Result := '6 - Microempres�rio e Empresa de Pequeno Porte (ME EPP)';
    retLucroReal                 : Result := '7 - Lucro Real';
    retLucroPresumido            : Result := '8 - Lucro Presumido';
    retSimplesNacional           : Result := '9 - Simples Nacional';
    retImune                     : Result := '10 - Imune';
    retEmpresaIndividualRELI     : Result := '11 - Empresa Individual de Resp. Limitada (EIRELI)';
    retEmpresaPP                 : Result := '12 - Empresa de Pequeno Porte (EPP)';
    retMicroEmpresario           : Result := '13 - Microempres�rio';
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
    rtIntermediario : Result := '2 - Intermedi�rio';
    rtPrestador     : Result := '3 - Prestador';
  else
    Result := '';
  end;
end;

function TACBrNFSeXProvider.NaturezaOperacaoDescricao(
  const t: TnfseNaturezaOperacao): string;
begin
  case t of
    no1 : Result := '1 - Tributa��o no munic�pio';
    no2 : Result := '2 - Tributa��o fora do munic�pio';
    no3 : Result := '3 - Isen��o';
    no4 : Result := '4 - Imune';
    no5 : Result := '5 - Exigibilidade susp. por decis�o judicial';
    no6 : Result := '6 - Exigibilidade susp. por proced. adm.';

    no51 : Result := '5.1 - Tributacao No Municipio com reten��o de ISS';
    no52 : Result := '9 - Tributacao No Municipio Sem Reten��o de ISS';
    no58 : Result := '5.8 - N�o tribut�vel';
    no59 : Result := '7 - Simples Nacional (Dentro Estado)';
    no61 : Result := '6.1 - Tributacao No Municipio Com Reten��o de ISS';
    no62 : Result := '6.2 - Tributacao No Municipio Sem Reten��o de ISS';
    no63 : Result := '6.3 - Tributa��o fora do municipio com reten��o de ISS';
    no64 : Result := '6.4 - Tributacao fora do municipio sem reten��o de ISS';
    no68 : Result := '6.8 - N�o tribut�vel';
    no69 : Result := '8 - Simples Nacional (Fora Estado)';
    no78 : Result := '7.8 - N�o tribut�vel';
    no79 : Result := '7.9 - Imposto recolhido pelo regime �nico de arrecada��o';

    no101 : Result := '101 - ISS devido no munic�pio';
    no103 : Result := '103 - ISENTO';
    no106 : Result := '106 - ISS FIXO';
    no107 : Result := '107 - ISS devido para o Municipio (Simples Nacional)';
    no108 : Result := '108 - ISS devido para outro Muinicipio (Simples Nacional)';
    no110 : Result := '110 - ISS retido pelo tomador devido para outros municipios (Simples Nacional)';
    no111 : Result := '111 - ISS RECOLHIDO NO PROJETO';
    no112 : Result := '112 - ISS N�O TRIBUT�VEL';
    no113 : Result := '113 - Nota Eletronica Avulsa';
    no114 : Result := '104 - ISS devido para origem prestado outro Munic�pio';
    no115 : Result := '115 - ISS devido para municipio, prestado em outro municipio';
    no121 : Result := '121 - ISS Fixo (Sociedade de Profissionais)';
    no201 : Result := '201 - ISS retido pelo tomador ou intermedi�rio do servi�o';
    no301 : Result := '301 - Opera��o imune, isenta ou n�o tributada';
    no501 : Result := '501 - ISS devido no munic�pio (Simples Nacional)';
    no511 : Result := '511 - Presta��o de servi�o no munic�pio - iss mensal sem reten��o na fonte';
    no512 : Result := '512 - Presta��o de servi�o no munic�pio - iss mensal com reten��o na fonte';
    no515 : Result := '515 - Presta��o de servi�o iss distribuido por rateio com reten��o na fonte';
    no521 : Result := '521 - Constru��o civil - no munic�pio - iss mensal sem reten��o na fonte';
    no522 : Result := '522 - Constru��o civil - no munic�pio - iss mensal com reten��o na fonte';
    no539 : Result := '539 - Prestacao de servi�o - recolhimento antecipado';
    no541 : Result := '541 - MEI (Simples Nacional)';
    no549 : Result := '549 - Prestacao de servi�o - isento ou imune - nao tributavel';
    no601 : Result := '601 - ISS retido pelo tomador ou intermedi�rio do servi�o (Simples Nacional)';
    no611 : Result := '611 - Presta��o de servi�o em outro munic�pio - iss mensal sem reten��o na fonte';
    no612 : Result := '612 - Presta��o de servi�o em outro munic�pio - iss mensal com reten��o na fonte';
    no613 : Result := '613 - Presta��o de servi�o em outro munic�pio - iss mensal devido no local da presta��o';
    no615 : Result := '615 - Presta��o de servi�o em outro munic�pio - devido em outro munic�pio - semreten��o na fonte';
    no621 : Result := '621 - Constru��o civil - outro munic�pio - iss mensal sem reten��o na fonte';
    no622 : Result := '622 - Constru��o civil - em outro munic�pio - iss mensal com reten��o na fonte';
    no701 : Result := '701 - Opera��o imune, isenta ou n�o tributada (Simples Nacional)';
    no711 : Result := '711 - Presta��o de servi�o para o exterior - iss mensal sem reten��o na fonte';
    no712 : Result := '712 - Presta��o de servi�o para o exterior - iss mensal com reten��o na fonte';
    no901 : Result := '901 - ISS retido ou sujeito � substitui��o tribut�ria devido no munic�pio';
    no902 : Result := '902 - ISS retido ou sujeito � substitui��o tribut�ria devido para outro munic�pio';
    no911 : Result := '911 - Presta��o de servi�o n�o enquadrada nas situa��es anteriores - sem reten��o';
    no912 : Result := '912 - Presta��o de servi�o n�o enquadrada nas situa��es anteriores - com reten��o';
    no921 : Result := '921 - ISS a ser recolhido pelo prestador do servi�o';
    no931 : Result := '931 - Servi�o imune, isento ou n�o tributado';
    no951 : Result := '951 - ISS retido ou sujeito � substitui��o tribut�ria no munic�pio (prestador optante pelo Simples Nacional)';
    no952 : Result := '952 - ISS retido ou sujeito � substitui��o tribut�ria, devido para outro munic�pio (prestador optante pelo Simples';
    no971 : Result := '971 - ISS a ser recolhido pelo prestador do servi�o (prestador optante pelo Simples Nacional)';
    no981 : Result := '981 - Servi�o imune, isento ou n�o tributado (prestador optante pelo Simples Nacional)';
    no991 : Result := '991 - Nota Fiscal de Servi�os Avulsa (ISS pago antecipadamente pelo prestador)';
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
    exiExigivel                       : Result := '1 - Exig�vel';
    exiNaoIncidencia                  : Result := '2 - N�o Incid�ncia';
    exiIsencao                        : Result := '3 - Isen��o';
    exiExportacao                     : Result := '4 - Exporta��o';
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
    ttNaoIncidencianoMunic: Result := 'E - N�o Incid�ncia no Munic�pio';
    ttImune               : Result := 'F - Imune';
    ttExigibilidadeSusp   : Result := 'K - Exigibilidade Susp.Dec.J/Proc.A';
    ttNaoTributavel       : Result := 'N - N�o Tribut�vel';
    ttTributavel          : Result := 'T - Tribut�vel';
    ttTributavelFixo      : Result := 'G - Tribut�vel Fixo';
    ttTributavelSN        : Result := 'H - Tribut�vel S.N.';
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

function TACBrNFSeXProvider.DeducaoPorToStr(const t: TDeducaoPor): string;
begin
  Result := EnumeradoToStr(t, ['', 'Percentual', 'Valor'], [dpNenhum, dpPercentual, dpValor]);
end;

function TACBrNFSeXProvider.StrToDeducaoPor(out ok: Boolean; const s: string): TDeducaoPor;
begin
  Result := StrToEnumerado(Ok, s, ['', 'Percentual', 'Valor'], [dpNenhum, dpPercentual, dpValor]);
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

function TACBrNFSeXProvider.StatusRPSToStr(const t: TStatusRPS): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [srNormal, srCancelado]);
end;

function TACBrNFSeXProvider.StrToStatusRPS(out ok: boolean;
  const s: string): TStatusRPS;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [srNormal, srCancelado]);
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
  GerarResponse.Sucesso := False;
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
    meLoteAssincrono: ValidarSchema(GerarResponse, tmRecepcionar);
    meLoteSincrono: ValidarSchema(GerarResponse, tmRecepcionarSincrono);
    meTeste: ValidarSchema(GerarResponse, tmTeste);
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
  GerarResponse.Sucesso := (GerarResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.Emite;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  EmiteResponse.Sucesso := False;
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
  EmiteResponse.Sucesso := (EmiteResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultaSituacao;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaSituacaoResponse.Sucesso := False;
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
  ConsultaSituacaoResponse.Sucesso := (ConsultaSituacaoResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultaLoteRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaLoteRpsResponse.Sucesso := False;
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
  ConsultaLoteRpsResponse.Sucesso := (ConsultaLoteRpsResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultaNFSeporRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaNFSeporRpsResponse.Sucesso := False;
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
  ConsultaNFSeporRpsResponse.Sucesso := (ConsultaNFSeporRpsResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultarEvento;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarEventoResponse.Sucesso := False;
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
  ConsultarEventoResponse.Sucesso := (ConsultarEventoResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultarParam;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarParamResponse.Sucesso := False;
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
  ConsultarParamResponse.Sucesso := (ConsultarParamResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultarDFe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarDFeResponse.Sucesso := False;
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
  ConsultarDFeResponse.Sucesso := (ConsultarDFeResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Prefixo: string;
begin
  ConsultaNFSeResponse.Sucesso := False;
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
  ConsultaNFSeResponse.Sucesso := (ConsultaNFSeResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultaLinkNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultaLinkNFSeResponse.Sucesso := False;
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
  ConsultaLinkNFSeResponse.Sucesso := (ConsultaLinkNFSeResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.CancelaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
//  aConfig: TConfiguracoesNFSe;
begin
  CancelaNFSeResponse.Sucesso := False;
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

//      aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);

//      AService.Path := aConfig.Arquivos.GetPathCan(0, aConfig.Geral.Emitente.CNPJ,
//                        aConfig.Geral.Emitente.DadosEmitente.InscricaoEstadual);

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
  CancelaNFSeResponse.Sucesso := (CancelaNFSeResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.SubstituiNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Cancelamento: TNFSeCancelaNFSeResponse;
  i: Integer;
begin
  SubstituiNFSeResponse.Sucesso := False;
  SubstituiNFSeResponse.Erros.Clear;
  SubstituiNFSeResponse.Alertas.Clear;
  SubstituiNFSeResponse.Resumos.Clear;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeSubstituicao);

  Cancelamento := TNFSeCancelaNFSeResponse.Create;

  try
    Cancelamento.Sucesso := False;
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
      NumeroNFSeSubst := SubstituiNFSeResponse.InfCancelamento.NumeroNFSeSubst;
      CodMunicipio := SubstituiNFSeResponse.InfCancelamento.CodMunicipio;
    end;

    PrepararCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      for i := 0 to Cancelamento.Erros.Count -1 do
      begin
        AErro := SubstituiNFSeResponse.Erros.New;
        AErro.Codigo := Cancelamento.Erros[i].Codigo;
        AErro.Descricao := Cancelamento.Erros[i].Descricao;
      end;

      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AssinarCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      for i := 0 to Cancelamento.Erros.Count -1 do
      begin
        AErro := SubstituiNFSeResponse.Erros.New;
        AErro.Codigo := Cancelamento.Erros[i].Codigo;
        AErro.Descricao := Cancelamento.Erros[i].Descricao;
      end;

      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    SubstituiNFSeResponse.PedCanc := Cancelamento.ArquivoEnvio;
    SubstituiNFSeResponse.PedCanc := SepararDados(SubstituiNFSeResponse.PedCanc, 'CancelarNfseEnvio', False);
    Cancelamento.Sucesso := True;
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
  SubstituiNFSeResponse.Sucesso := (SubstituiNFSeResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.GerarToken;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  GerarTokenResponse.Sucesso := False;
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
  GerarTokenResponse.Sucesso := (GerarTokenResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.EnviarEvento;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  aConfig: TConfiguracoesNFSe;
begin
  EnviarEventoResponse.Sucesso := False;
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
  EnviarEventoResponse.Sucesso := (EnviarEventoResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.ConsultarSeqRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  ConsultarSeqRpsResponse.Sucesso := False;
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
  ConsultarSeqRpsResponse.Sucesso := (ConsultarSeqRpsResponse.Erros.Count = 0);
end;

procedure TACBrNFSeXProvider.AssinarConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                      ConfigMsgDados.ConsultarLinkNFSe.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarLinkNFSe.DocElemento,
      ConfigMsgDados.ConsultarLinkNFSe.InfElemento, '', '', '', IdAttr,
      IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                      ConfigMsgDados.CancelarNFSe.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.CancelarNFSe.DocElemento,
      ConfigMsgDados.CancelarNFSe.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                      ConfigMsgDados.ConsultarLote.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarLote.DocElemento,
      ConfigMsgDados.ConsultarLote.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
        begin
          IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                      ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento, IdAttr);

          Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
            Prefixo + ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento,
            ConfigMsgDados.ConsultarNFSePorFaixa.InfElemento, '', '', '', IdAttr,
            IdAttrSig);
        end;

      tcServicoPrestado:
        begin
          IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
               ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento, IdAttr);

          Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
            Prefixo + ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento,
            ConfigMsgDados.ConsultarNFSeServicoPrestado.InfElemento, '', '', '',
            IdAttr, IdAttrSig);
        end;

      tcServicoTomado:
        begin
          IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                 ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento, IdAttr);

          Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
            Prefixo + ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento,
            ConfigMsgDados.ConsultarNFSeServicoTomado.InfElemento, '', '', '',
            IdAttr, IdAttrSig);
        end
    else
      begin
        IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                        ConfigMsgDados.ConsultarNFSe.DocElemento, IdAttr);

        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.ConsultarNFSe.DocElemento,
          ConfigMsgDados.ConsultarNFSe.InfElemento, '', '', '', IdAttr,
          IdAttrSig);
      end;
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                           ConfigMsgDados.ConsultarNFSeRps.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSeRps.DocElemento,
      ConfigMsgDados.ConsultarNFSeRps.InfElemento, '', '', '', IdAttr,
      IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.ConsultarEvento.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarEvento.DocElemento,
      ConfigMsgDados.ConsultarEvento.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.ConsultarParam.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarParam.DocElemento,
      ConfigMsgDados.ConsultarParam.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.ConsultarSituacao.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarSituacao.DocElemento,
      ConfigMsgDados.ConsultarSituacao.InfElemento, '', '', '', IdAttr,
      IdAttrSig);
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
  IdAttr, Prefixo, PrefixoTS, IdAttrSig: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if (Response.ModoEnvio = meTeste) and
     (not GetConfigGeral.ServicosDisponibilizados.TestarEnvio) then
    Exit;

  if (Response.ModoEnvio = meLoteSincrono) and
     (not GetConfigGeral.ServicosDisponibilizados.EnviarLoteSincrono) then
    Exit;

  if (Response.ModoEnvio = meLoteAssincrono) and
     (not GetConfigGeral.ServicosDisponibilizados.EnviarLoteAssincrono) then
    Exit;

  if (Response.ModoEnvio = meUnitario) and
     (not GetConfigGeral.ServicosDisponibilizados.EnviarUnitario) then
    Exit;

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
        begin
          IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.LoteRpsSincrono.DocElemento, IdAttr);

          Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
            Prefixo + ConfigMsgDados.LoteRpsSincrono.DocElemento,
            Prefixo + ConfigMsgDados.LoteRpsSincrono.InfElemento, '', '', '',
            IdAttr, IdAttrSig);
        end;

      meTeste,
      meLoteAssincrono:
        begin
          IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                                    ConfigMsgDados.LoteRps.DocElemento, IdAttr);

          Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
            Prefixo + ConfigMsgDados.LoteRps.DocElemento,
            {Prefixo + }ConfigMsgDados.LoteRps.InfElemento, '', '', '',
            IdAttr, IdAttrSig);
        end
    else
      begin
        IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                                  ConfigMsgDados.GerarNFSe.DocElemento, IdAttr);

        Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
          Prefixo + ConfigMsgDados.GerarNFSe.DocElemento,
          PrefixoTS + ConfigMsgDados.GerarNFSe.InfElemento, '', '', '',
          IdAttr, IdAttrSig);
      end;
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.SubstituirNFSe.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.SubstituirNFSe.DocElemento,
      ConfigMsgDados.SubstituirNFSe.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.GerarToken.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.GerarToken.DocElemento,
      ConfigMsgDados.GerarToken.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.EnviarEvento.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.EnviarEvento.DocElemento,
      ConfigMsgDados.EnviarEvento.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.ConsultarDFe.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarDFe.DocElemento,
      ConfigMsgDados.ConsultarDFe.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                            ConfigMsgDados.ConsultarParam.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarParam.DocElemento,
      ConfigMsgDados.ConsultarParam.InfElemento, '', '', '', IdAttr, IdAttrSig);
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
