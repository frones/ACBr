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

unit Pronim.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv1, ACBrNFSeXGravarXml_ABRASFv2,
  ACBrNFSeXConversao;

type
  { TNFSeW_Pronim }

  TNFSeW_Pronim = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

  end;

  { TNFSeW_Pronim202 }

  TNFSeW_Pronim202 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  public
    function GerarXml: Boolean; Override;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Pronim
//==============================================================================

{ TNFSeW_Pronim }

procedure TNFSeW_Pronim.Configuracao;
begin
  inherited Configuracao;

  NrOcorrOutrasRet := 1;
  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrBaseCalc := 1;
  NrOcorrAliquota := 1;
  NrOcorrValorISSRetido_1 := -1;
  NrOcorrValorISSRetido_2 := 0;
end;

{ TNFSeW_Pronim202 }

procedure TNFSeW_Pronim202.Configuracao;
begin
  inherited Configuracao;

  NrOcorrAliquota := 0;

  Opcoes.SuprimirDecimais := True;

  if FpAOwner.ConfigGeral.Params.TemParametro('NaoSuprimirDecimais') then
    Opcoes.SuprimirDecimais := False;
end;

function TNFSeW_Pronim202.GerarXml: Boolean;
const
  CODIGOMUNICIPIO_EXTERIOR = '9999999';
begin
  if NFSe.OptanteSimplesNacional = snSim then
    NrOcorrAliquota := 1;

  // Solução para o erro "Responsável/Retentor informado indevido. (E282)"
  // quando ISSQN não é retido na fonte
  if NFSe.Servico.Valores.IssRetido <> stRetencao then
    NrOcorrRespRetencao := -1;

  // Solução para o erro "País do tomador do serviço indevido. (E292)"
  // quando tomador não é estrangeiro
  if NFSe.Tomador.Endereco.CodigoMunicipio <> CODIGOMUNICIPIO_EXTERIOR then
    NrOcorrCodigoPaisTomador := -1;

  Result := inherited GerarXml;
end;

end.
