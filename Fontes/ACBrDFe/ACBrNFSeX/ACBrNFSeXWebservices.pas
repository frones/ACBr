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

unit ACBrNFSeXWebservices;

interface

uses
  Classes, SysUtils,
  ACBrNFSeXWebservicesResponse;

type

  { TWebServices }
  TWebServices = class
  private
    FGerar: TNFSeEmiteResponse;
    FEmite: TNFSeEmiteResponse;
    FConsultaSituacao: TNFSeConsultaSituacaoResponse;
    FConsultaLoteRps: TNFSeConsultaLoteRpsResponse;
    FConsultaNFSeporRps: TNFSeConsultaNFSeporRpsResponse;
    FConsultaNFSe: TNFSeConsultaNFSeResponse;
    FConsultaLinkNFSe: TNFSeConsultaLinkNFSeResponse;
    FCancelaNFSe: TNFSeCancelaNFSeResponse;
    FSubstituiNFSe: TNFSeSubstituiNFSeResponse;
    FGerarToken: TNFSeGerarTokenResponse;
    FEnviarEvento: TNFSeEnviarEventoResponse;
    FConsultarEvento: TNFSeConsultarEventoResponse;
    FConsultarDFe: TNFSeConsultarDFeResponse;
    FConsultarParam: TNFSeConsultarParamResponse;
    FConsultarSeqRps: TNFSeConsultarSeqRpsResponse;

  public
    constructor Create;
    destructor Destroy; override;

    property Gerar: TNFSeEmiteResponse read FGerar;
    property Emite: TNFSeEmiteResponse read FEmite;
    property ConsultaSituacao: TNFSeConsultaSituacaoResponse read FConsultaSituacao;
    property ConsultaLoteRps: TNFSeConsultaLoteRpsResponse read FConsultaLoteRps;
    property ConsultaNFSeporRps: TNFSeConsultaNFSeporRpsResponse read FConsultaNFSeporRps;
    property ConsultaNFSe: TNFSeConsultaNFSeResponse read FConsultaNFSe;
    property ConsultaLinkNFSe: TNFSeConsultaLinkNFSeResponse read FConsultaLinkNFSe;
    property CancelaNFSe: TNFSeCancelaNFSeResponse read FCancelaNFSe;
    property SubstituiNFSe: TNFSeSubstituiNFSeResponse read FSubstituiNFSe;
    property GerarToken: TNFSeGerarTokenResponse read FGerarToken;
    property EnviarEvento: TNFSeEnviarEventoResponse read FEnviarEvento;
    property ConsultarEvento: TNFSeConsultarEventoResponse read FConsultarEvento;
    property ConsultarDFe: TNFSeConsultarDFeResponse read FConsultarDFe;
    property ConsultarParam: TNFSeConsultarParamResponse read FConsultarParam;
    property ConsultarSeqRps: TNFSeConsultarSeqRpsResponse read FConsultarSeqRps;

  end;

implementation

{ TWebServices }
constructor TWebServices.Create;
begin
  FGerar := TNFSeEmiteResponse.Create;
  FEmite := TNFSeEmiteResponse.Create;
  FConsultaSituacao := TNFSeConsultaSituacaoResponse.Create;
  FConsultaLoteRps := TNFSeConsultaLoteRpsResponse.Create;
  FConsultaNFSeporRps := TNFSeConsultaNFSeporRpsResponse.Create;
  FConsultaNFSe := TNFSeConsultaNFSeResponse.Create;
  FConsultaLinkNFSe := TNFSeConsultaLinkNFSeResponse.Create;
  FCancelaNFSe := TNFSeCancelaNFSeResponse.Create;
  FSubstituiNFSe := TNFSeSubstituiNFSeResponse.Create;
  FGerarToken := TNFSeGerarTokenResponse.Create;
  FEnviarEvento := TNFSeEnviarEventoResponse.Create;
  FConsultarEvento := TNFSeConsultarEventoResponse.Create;
  FConsultarDFe := TNFSeConsultarDFeResponse.Create;
  FConsultarParam := TNFSeConsultarParamResponse.Create;
  FConsultarSeqRps := TNFSeConsultarSeqRpsResponse.Create;
end;

destructor TWebServices.Destroy;
begin
  FGerar.Free;
  FEmite.Free;
  FConsultaSituacao.Free;
  FConsultaLoteRps.Free;
  FConsultaNFSeporRps.Free;
  FConsultaNFSe.Free;
  FConsultaLinkNFSe.Free;
  FCancelaNFSe.Free;
  FSubstituiNFSe.Free;
  FGerarToken.Free;
  FEnviarEvento.Free;
  FConsultarEvento.Free;
  FConsultarDFe.Free;
  FConsultarParam.Free;
  FConsultarSeqRps.Free;

  inherited Destroy;
end;

end.
