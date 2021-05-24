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
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrNFSeXConversao, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXWebserviceBase, ACBrNFSeXParametros;

type
  IACBrNFSeXProvider = interface ['{6A71A59C-9EA1-45BF-BCAB-59BB90B62AAA}']
    function GerarXml(const ANFSe: TNFSe; var AXml, AAlerts: string): Boolean;
    function LerXml(const AXML: String; var ANFSe: TNFSe): Boolean;

    function GeraLote(const aLote: String; aqMaxRps: Integer; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
    function Emite(const ALote: String; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
    function ConsultaSituacao(const AProtocolo, ANumLote: String): TNFSeConsultaSituacaoResponse;
    function ConsultaLoteRps(const AProtocolo, ANumLote: String): TNFSeConsultaLoteRpsResponse;
    function ConsultaNFSeporRps(const ANumRPS, ASerie, ATipo,
                                ACodVerificacao: String): TNFSeConsultaNFSeporRpsResponse;
    function ConsultaNFSe(aInfConsultaNFSe: TInfConsultaNFSe): TNFSeConsultaNFSeResponse;
    function CancelaNFSe(aInfCancelamento: TInfCancelamento): TNFSeCancelaNFSeResponse;
    function SubstituiNFSe(const ANumNFSe, ACodCancelamento, AMotCancelamento, ANumLote,
                           ACodVerificacao: String): TNFSeSubstituiNFSeResponse;

    function GetConfigGeral: TConfigGeral;
    function GetConfigWebServices: TConfigWebServices;
    function GetConfigMsgDados: TConfigMsgDados;
    function GetConfigAssinar: TConfigAssinar;
    function GetConfigSchemas: TConfigSchemas;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;
  end;

implementation

end.
