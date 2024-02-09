{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Dias                                     }
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

unit ACBrNFSeXInterface;

interface

uses
  ACBrNFSeXClass, ACBrNFSeXParametros, ACBrNFSeXConversao;

type
  IACBrNFSeXProvider = interface ['{6A71A59C-9EA1-45BF-BCAB-59BB90B62AAA}']
    function GerarXml(const ANFSe: TNFSe; var AXml, AAlerts: string): Boolean;
    function LerXml(const AXML: String; var ANFSe: TNFSe; var ATipo: TtpXML;
      var aXmlTratado: string): Boolean;

    procedure GeraLote;
    procedure Emite;
    procedure ConsultaSituacao;
    procedure ConsultaLoteRps;
    procedure ConsultaNFSeporRps;
    procedure ConsultaNFSe;
    procedure ConsultaLinkNFSe;
    procedure CancelaNFSe;
    procedure SubstituiNFSe;
    procedure GerarToken;
    procedure EnviarEvento;
    procedure ConsultarEvento;
    procedure ConsultarDFe;
    procedure ConsultarParam;
    procedure ConsultarSeqRps;

    function GetConfigGeral: TConfigGeral;
    function GetConfigWebServices: TConfigWebServices;
    function GetConfigMsgDados: TConfigMsgDados;
    function GetConfigAssinar: TConfigAssinar;
    function GetConfigSchemas: TConfigSchemas;
    function GetSchemaPath: string;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;

    function SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string;
    function StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps;
    function SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string;

    function SimNaoToStr(const t: TnfseSimNao): string;
    function StrToSimNao(out ok: boolean; const s: string): TnfseSimNao;
    function SimNaoDescricao(const t: TnfseSimNao): string;

    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string;

    function SituacaoTributariaToStr(const t: TnfseSituacaoTributaria): string;
    function StrToSituacaoTributaria(out ok: boolean; const s: string): TnfseSituacaoTributaria;
    function SituacaoTributariaDescricao(const t: TnfseSituacaoTributaria): string;

    function ResponsavelRetencaoToStr(const t: TnfseResponsavelRetencao): string;
    function StrToResponsavelRetencao(out ok: boolean; const s: string): TnfseResponsavelRetencao;
    function ResponsavelRetencaoDescricao(const t: TnfseResponsavelRetencao): String;

    function NaturezaOperacaoDescricao(const t: TnfseNaturezaOperacao): string; 

    function TipoPessoaToStr(const t: TTipoPessoa): string;
    function StrToTipoPessoa(out ok: boolean; const s: string): TTipoPessoa;

    function ExigibilidadeISSToStr(const t: TnfseExigibilidadeISS): string;
    function StrToExigibilidadeISS(out ok: boolean; const s: string): TnfseExigibilidadeISS;
    function ExigibilidadeISSDescricao(const t: TnfseExigibilidadeISS): string;

    function TipoRPSToStr(const t:TTipoRPS): string;
    function StrToTipoRPS(out ok: boolean; const s: string): TTipoRPS;

    function SituacaoTribToStr(const t: TSituacaoTrib): string;
    function StrToSituacaoTrib(out ok: boolean; const s: string): TSituacaoTrib;

    function TributacaoToStr(const t: TTributacao): string;
    function StrToTributacao(out ok: boolean; const s: string): TTributacao;
    function TributacaoDescricao(const t: TTributacao): String;

    function TipoDeducaoToStr(const t: TTipoDeducao): string;
    function StrToTipoDeducao(out ok: Boolean; const s: string): TTipoDeducao;

    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS;

    function CondicaoPagToStr(const t: TnfseCondicaoPagamento): string;
    function StrToCondicaoPag(out ok: boolean; const s: string): TnfseCondicaoPagamento;

    function StatusRPSToStr(const t: TStatusRPS): string;
    function StrToStatusRPS(out ok: boolean; const s: string): TStatusRPS;
  end;

implementation

end.
